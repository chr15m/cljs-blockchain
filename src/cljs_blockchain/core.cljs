(ns cljs-blockchain.core
  (:require
    [cljs.reader] 
    [cljs.core.async :refer [chan <! timeout put!]]
    ["bencode-js/lib/index" :as bencode]
    ["scrypt-js/scrypt" :as scrypt]
    [cljsjs.webtorrent :as wt]
    [cljsjs.nacl-fast :as nacl]
    [reagent.core :as r])
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))

; TODO:
; * add index to blocks
; * block headers: version, previous-block-hash, transaction-merkle-root-hash, timestamp, difficulty, nonce
;   https://en.bitcoin.it/wiki/Block_hashing_algorithm
; * sign transactions
; * include nonce in coinbase transaction (to update merkle root)

(defonce state (r/atom {:incoming (chan)}))

;*** utility fns ***;

(def ms-per-day (* 1000 60 60 24))

(def storage (aget js/window "localStorage"))

(defn now []
  (-> (js/Date.) (.getTime)))

(defn compute-epoch [t]
  (-> t (/ ms-per-day) (int)))

(defn has-epoch-changed [epoch blockchain]
  (not= epoch (get-in blockchain [0 :epoch])))

;*** data manipulation ***;

(defn to-hex [b]
  (.join (.map (js/Array.from (.slice b)) #(.slice (str "0" (.toString (bit-and % 0xff) 16)) -2)) ""))

(defn from-hex [b]
  (js/Uint8Array. (map #(js/parseInt (apply str %) 16) (partition 2 b))))

(defn fingerprint [x]
  (-> x
      (to-hex)
      (.substring 0 8)))

(defn pk [state]
  (-> state
      :keypair
      (or #js {})
      (.. -publicKey)
      (or #js [])
      (to-hex)))

;*** blockchain ***;

(defn blockchain-make-genesis-block [t]
  {:timestamp 0
   :transactions #{}
   :epoch (compute-epoch t)
   ; one reason why you should not use this for anything serious:
   ; somebody can generate an entire epoch worth of blocks
   ; for some future block, winning all mining rewards for the block
   ; because the following value is deterministic and predictable
   :pow (nacl.hash (js/Uint8Array.from (str "cljs-blockchain #" (compute-epoch t))))
   :nonce 0
   :index 0
   :previous-hash 0x1})

(defn blockchain-make-block [t transactions previous-hash new-index]
  (let [new-hash (-> previous-hash
                     (to-hex)
                     (str "-next")
                     (js/Uint8Array.from)
                     (nacl.hash))]
    {:timestamp t
     :transactions transactions
     :epoch (compute-epoch t)
     :pow new-hash
     :nonce 0
     :index new-index
     :previous-hash previous-hash}))

(defn add-block-to-blockchain [new-state]
  ; split top ten transactions by fee off mempool
  (let [mempool-by-fee (reverse (sort-by :fee (new-state :mempool)))
        [transactions mempool-remaining] (split-at 10 mempool-by-fee)
        previous-hash (-> new-state :blockchain (last) :pow)
        new-index (-> new-state :blockchain (count))]
    (-> new-state
        (update-in [:blockchain] conj (blockchain-make-block (now) transactions previous-hash new-index))
        (assoc :mempool mempool-remaining))))

(defn add-transaction-to-mempool [new-state {:keys [to from amount]}]
  (print "adding transaction:" to from amount)
  ; TODO: regex check to is hex
  ; TODO: regex check from is hex
  ; TODO: regex check amount is number
  (let [to (from-hex to)
        from (from-hex from)
        amount (int amount)]
    (print (bencode/encode #js {:to to :from from :amount amount}))
    ; TODO: also check balance
    ; TODO: also check from address is own
    (if (and
          (= (.-length to) 32)
          (= (.-length from) 32)
          (> amount 0))
      (update-in new-state [:mempool] conj {:to to :from from :amount amount :fee 0})
      new-state)))

(defn transaction-hash [t]
  (nacl.hash (Uint8Array.from (bencode/encode (clj->js t)))))

;*** crypto ***;

(defn ensure-keypair! []
  (let [k (cljs.reader/read-string (.getItem storage "secret-key"))
        k (if k (nacl.sign.keyPair.fromSecretKey (js/Uint8Array. k)) (nacl.sign.keyPair))]
    (.setItem storage "secret-key" (prn-str (js/Array.from (.-secretKey k))))
    k))

;*** main event loop ***;

(defn process-event [new-state event]
  (cond
    (event :add-transaction) (add-transaction-to-mempool new-state (event :add-transaction))
    (event :add-block) (add-block-to-blockchain new-state)
    :else new-state))

(defn main-loop [old-state]
  (go
    (let [new-state old-state
          ; wait for events
          event (<! (new-state :incoming))
          ; compute current epoch
          current-epoch (compute-epoch (now))
          ; ensure blockchain structure
          new-state (update-in new-state [:blockchain] #(if (has-epoch-changed current-epoch %) [] %))
          ; ensure genesis block
          new-state (update-in new-state [:blockchain 0] #(or % (blockchain-make-genesis-block (now))))
          ; ensure mempool structure
          new-state (update-in new-state [:mempool] #(or % #{}))
          ; ensure webtorrent connection
          ; process events
          new-state (try (process-event new-state event) (catch :default e (do (js/console.error e) new-state)))]
      (js/console.log "main-loop new-state:" (clj->js new-state))
      new-state)))

(defn check-epoch [state]
  (when (has-epoch-changed (compute-epoch (now)) (@state :blockchain))
    (put! (@state :incoming) :update-epoch)))

;; -------------------------
;; User interface

(defn submit-transaction [state interface]
  (put! (@state :incoming) {:add-transaction
                            {:to (@interface :to)
                             :from (pk @state)
                             :amount (int (@interface :amount))
                             :fee (int (@interface :fee))}})
  (reset! interface {}))

(defn submit-block [state]
  (put! (@state :incoming) {:add-block true}))

(defn home-page [state]
  ; TODO: transaction validation
  (let [interface (r/atom {})
        to (r/cursor interface [:to])
        amount (r/cursor interface [:amount])
        fee (r/cursor interface [:fee])]
    (fn []
      [:div
       [:div#header
        [:h2 "cljs-blockchain"]
        [:p [:small "provided 'as-is' without warranty of any kind. " [:strong "this is a toy"] "."]]
        [:p [:small "this blockchain resets every 24 hrs, deleting all history."]]]
       [:div#user
        [:h3 "wallet"]
        [:p "public key: " [:code (pk @state)]]
        [:p "balance: " 0]]
       [:div#ui
        [:h3 "make transaction"]
        [:input {:placeholder "to public key" :on-change #(reset! to (-> % .-target .-value)) :value @to}]
        [:input {:placeholder "amount" :type "number" :on-change #(reset! amount (-> % .-target .-value)) :value @amount}]
        [:input {:placeholder "fee" :type "number" :on-change #(reset! fee (-> % .-target .-value)) :value @fee}]
        ; [:input {:placeholder "message"}]
        [:button {:on-click (partial submit-transaction state interface)} "Send"]]
       [:div#mining
        [:h3 "mining"]
        [:button {:on-click (partial submit-block state)} "mine a block"]]
       [:div#stats
        [:h3 "stats"]
        [:table
         [:tbody
          [:tr
           [:td "peers"]
           [:td 0]]
          [:tr
           [:td "mempool size"]
           [:td (count (@state :mempool))]]
          [:tr
           [:td "difficulty"]
           [:td 0]]
          [:tr
           [:td "fee range"]
           [:td 0 " -> " 0]]
          [:tr
           [:td "block epoch"]
           [:td (get-in @state [:blockchain 0 :epoch])]]]]]
       [:div#blockchain
        [:h3 "blockchain history"]
        (for [b (reverse (@state :blockchain))]
          [:div.block {:key (fingerprint (b :pow))}
           [:strong "block: " (fingerprint (b :pow)) " (" (inc (b :index)) ")"]
           (if (= (b :previous-hash) 0x1)
             [:div.transaction "genesis block"]
             (for [t (b :transactions)]
               [:div.transaction {:key (fingerprint (transaction-hash t))}
                [:div (fingerprint (t :to)) " -> " (fingerprint (t :from))
                 [:span.amount (t :amount)]
                 [:span.fee "fee: " (t :fee)]]]))])]])))

;; -------------------------
;; Initialize app

(defn mount-root []
  (put! (@state :incoming) :init)
  (r/render [home-page state] (.getElementById js/document "app")))

(defn init! []
  (swap! state assoc :keypair (ensure-keypair!))
  ; app state mutation happens here
  (go-loop [] (reset! state (<! (main-loop @state))) (recur))
  ; check for blockchain epoch change (reset chain daily) every second
  (js/setInterval #(check-epoch state) 1000)
  (mount-root))

