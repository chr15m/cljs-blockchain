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
; * use localStorage window storage event to pass transactions and blocks around
; * validate transactions when added
; * validate blockchain when a block is added
; * compute POW at mining stage

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

;*** blockchain ***;

(defn blockchain-make-genesis-block []
  {:timestamp 0
   :transactions #{}
   :hash (nacl.hash (js/Uint8Array.from (str "cljs-blockchain-ftw")))
   :nonce 0
   :index 0
   :previous-hash 0x1})

(defn blockchain-make-block [t transactions previous-hash new-index]
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

(defn add-block-to-blockchain [new-state]
  ; split top ten transactions by fee off mempool
  (let [mempool-by-fee (reverse (sort-by :fee (new-state :mempool)))
        [transactions mempool-remaining] (split-at 10 mempool-by-fee)
        previous-hash (-> new-state :blockchain (last) :hash)
        new-index (-> new-state :blockchain (count))]
    (-> new-state
        (update-in [:blockchain] conj (blockchain-make-block (now) transactions previous-hash new-index))
        (assoc :mempool mempool-remaining))))

(defn add-transaction-to-mempool [new-state {:keys [to from amount fee]}]
  (print "adding transaction:" to from amount)
  ; TODO: regex check to is hex
  ; TODO: regex check from is hex
  ; TODO: regex check amount is number
  (let [to (from-hex to)
        from (from-hex from)
        amount (int amount)
        transaction {:to to :from from :amount amount :fee fee}
        signature (nacl.sign.detached
                    (->> transaction
                         (clj->js)
                         (bencode/encode)
                         (Uint8Array.from))
                    (-> new-state
                        :keypair
                        (or #js {})
                        (.. -secretKey)))
        transaction (assoc transaction :signature signature)]
    ; TODO: also check balance
    ; TODO: also check from address is own
    (if (and
          (= (.-length to) 32)
          (= (.-length from) 32)
          (> amount 0))
      (update-in new-state [:mempool] conj transaction)
      new-state)))

;*** crypto ***;

(defn ensure-keypair! []
  (let [k (cljs.reader/read-string (.getItem storage "secret-key"))
        k (if k (nacl.sign.keyPair.fromSecretKey (js/Uint8Array. k)) (nacl.sign.keyPair))]
    (.setItem storage "secret-key" (prn-str (js/Array.from (.-secretKey k))))
    k))

;; -------------------------
;; User interface

(defn submit-transaction! [state interface]
  (swap! state add-transaction-to-mempool
         {:to (@interface :to)
          :from (pk @state)
          :amount (int (@interface :amount))
          :fee (int (@interface :fee))})
  (reset! interface {}))

(defn submit-block! [state]
  (swap! state add-block-to-blockchain))

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
        [:p [:small "provided 'as-is' without warranty of any kind. " [:strong "this is a toy"] "."]]]
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
        [:button {:on-click (partial submit-transaction! state interface)} "Send"]]
       [:div#mining
        [:h3 "mining"]
        [:button {:on-click (partial submit-block! state)} "mine a block"]]
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
           [:td 0 " -> " 0]]]]]
       [:div#blockchain
        [:h3 "blockchain history"]
        (for [b (reverse (@state :blockchain))]
          [:div.block {:key (fingerprint (b :hash))}
           [:strong "block: " (fingerprint (b :hash)) " (" (inc (b :index)) ")"]
           (if (= (b :previous-hash) 0x1)
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
         :blockchain [(blockchain-make-genesis-block)]
         :mempool #{})
  ; app state mutation happens here
  (mount-root))

