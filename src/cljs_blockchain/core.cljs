(ns cljs-blockchain.core
  (:require
    [cljs.reader] 
    ["bencode-js/lib/index" :as bencode]
    ["scrypt-js/scrypt" :as scrypt]
    [cljsjs.webtorrent :as wt]
    [cljsjs.nacl-fast :as nacl]
    [reagent.core :as r]))

; TODO:
; * use localStorage window storage event to pass transactions and blocks around
; * validate transactions when added
; * validate blockchain when a block is added
; * add coinbase transaction
; * function to compute total
; * compute POW at mining stage
; * UI validation

(defonce state (r/atom {}))

;*** utility fns ***;

(def ms-per-day (* 1000 60 60 24))

(def storage (aget js/window "localStorage"))

(defn now []
  (-> (js/Date.) (.getTime)))

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

(defn hash-object [t]
  (nacl.hash (Uint8Array.from (bencode/encode (clj->js t)))))

;*** crypto ***;

(defn ensure-keypair! []
  (let [k (cljs.reader/read-string (.getItem storage "secret-key"))
        k (if k (nacl.sign.keyPair.fromSecretKey (js/Uint8Array. k)) (nacl.sign.keyPair))]
    (.setItem storage "secret-key" (prn-str (js/Array.from (.-secretKey k))))
    k))

;*** blockchain ***;

(def genesis-hash
  (nacl.hash (js/Uint8Array.from (str "cljs-blockchain-ftw"))))

(defn validate-transaction [transaction]
  ; TODO: check balance
  ; TODO: check from address is own
  ; TODO: check signature
  (and
        (= (.-length (transaction :to)) 32)
        (= (.-length (transaction :from)) 32)
        (> (transaction :amount) 0)))

(defn make-block [t transactions previous-hash new-index]
  (let [nonce (nacl.randomBytes 8)
        new-hash (hash-object [(to-hex previous-hash)
                               t
                               (to-hex (hash-object transactions))
                               (to-hex nonce)])]
    {:timestamp t
     :transactions transactions
     :hash new-hash
     :nonce nonce
     :index new-index
     :previous-hash previous-hash}))

(defn make-transaction [keypair to from amount fee]
  (let [transaction {:to (from-hex to) :from (from-hex from) :amount (int amount) :fee (int fee)}
        signature (nacl.sign.detached
                    (->> transaction
                         (clj->js)
                         (bencode/encode)
                         (Uint8Array.from))
                    (.. keypair -secretKey))]
    (assoc transaction :signature signature)))

(defn mine-block [new-state]
  ; split top ten transactions by fee off mempool
  (let [mempool-by-fee (reverse (sort-by :fee (new-state :mempool)))
        [transactions mempool-remaining] (split-at 10 mempool-by-fee)
        previous-hash (-> new-state :blockchain (last) :hash)
        new-index (-> new-state :blockchain (count))
        new-block (loop [c 0]
                    (let [candidate-block (make-block (now) transactions previous-hash new-index)]
                      ; (js/console.log "candidate-block" c (clj->js candidate-block))
                      ; find a block with one byte of leading zeros (fixed difficulty)
                      (if (not= (aget (candidate-block :hash) 0) 0)
                        (recur (inc c))
                          candidate-block)))]
    (-> new-state
        (update-in [:blockchain] conj new-block)
        (assoc :mempool mempool-remaining))))

(defn add-transaction-to-mempool [new-state transaction]
  (if (validate-transaction transaction) 
    (update-in new-state [:mempool] conj transaction)
    new-state))

(defn median [& ar]
  (let [l (count ar)]
    (when (> l 0)
      (nth (sort ar) (int (/ l 2))))))

(defn fee-calc [state f default]
  (or
    (apply f (map :fee (state :mempool)))
    default))

;; -------------------------
;; User interface

(defn submit-transaction! [state interface]
  (let [transaction (make-transaction
                      (@state :keypair)
                      (@interface :to)
                      (pk @state)
                      (int (@interface :amount))
                      (int (@interface :fee)))]
    (swap! state add-transaction-to-mempool transaction))
  (reset! interface {}))

(defn submit-block! [state miner-ui]
  (reset! miner-ui "Mining a block...")
  (swap! state mine-block)
  (reset! miner-ui (str "Found block: " (-> @state :blockchain last :hash (fingerprint)))))

(defn copy-pk []
  (let [pk-el (js/document.getElementById "pk")]
    (.focus pk-el)
    (.select pk-el)
    (.execCommand js/document "copy")))

(defn home-page [state]
  ; TODO: transaction validation
  (let [interface (r/atom {})
        to (r/cursor interface [:to])
        amount (r/cursor interface [:amount])
        fee (r/cursor interface [:fee])
        miner-ui (r/cursor interface [:miner])]
    (fn []
      [:div
       [:div#header
        [:h2 "cljs-blockchain"]
        [:p [:small "provided 'as-is' without warranty of any kind. " [:strong "this is a toy"] "."]]]
       [:div#user
        [:h3 "wallet"]
        [:p "public key: " [:input#pk {:value (pk @state) :readOnly true}] [:button {:on-click copy-pk} "copy"]]
        [:p "balance: " 0]]
       [:div#ui
        [:h3 "make transaction"]
        [:input {:placeholder "to public key" :on-change #(reset! to (.replace (-> % .-target .-value) #"[^A-Fa-f0-9]" "")) :value @to}]
        [:input {:placeholder "amount" :on-change #(reset! amount (.replace (-> % .-target .-value) #"[^0-9]" "")) :value @amount}]
        [:input {:placeholder "fee" :on-change #(reset! fee (.replace (-> % .-target .-value) #"[^0-9]" "")) :value (or @fee (fee-calc @state median nil))}]
        ; [:input {:placeholder "message"}]
        [:button {:on-click (partial submit-transaction! state interface)} "Send"]]
       [:div#mining
        [:h3 "mining"]
        [:button {:on-click (partial submit-block! state miner-ui)} "mine a block"]
        [:span#mining @miner-ui]]
       [:div#stats
        [:h3 "mempool"]
        [:table
         [:tbody
          [:tr
           [:td "mempool size"]
           [:td (count (@state :mempool))]]
          [:tr
           [:td "mempool fee range"]
           [:td (fee-calc @state min 0) " -> " (fee-calc @state max 0)]]]]]
       [:div#blockchain
        [:h3 "blockchain history"]
        (for [b (reverse (@state :blockchain))]
          [:div.block {:key (fingerprint (b :hash))}
           [:strong "block: " (fingerprint (b :hash)) " (" (inc (b :index)) ")"]
           (if (= (b :index) 0)
             [:div.transaction "genesis block"]
             (for [t (b :transactions)]
               [:div.transaction {:key (fingerprint (hash-object t))}
                [:div (fingerprint (t :to)) " -> " (fingerprint (t :from))
                 [:span.amount (t :amount)]
                 [:span.fee "fee: " (t :fee)]
                 [:span.signature "signature: " (fingerprint (t :signature))]]]))])]])))

;; -------------------------
;; Initialize app

(defn mount-root []
  (r/render [home-page state] (.getElementById js/document "app")))

(defn init! []
  (swap! state assoc
         :keypair (ensure-keypair!)
         :blockchain [(make-block 0 #{} genesis-hash 0)]
         :mempool #{})
  ; app state mutation happens here
  (mount-root))

