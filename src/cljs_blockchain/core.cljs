(ns cljs-blockchain.core
  (:require
    ["bencode-js/lib/index" :as bencode]
    [cljsjs.nacl-fast :as nacl]
    [reagent.core :as r]))

(def coinbase-from "00000000000000000000000000000000")
(def block-reward 10)

;; -------------------------
;; Utility fns

(defn now []
  (-> (js/Date.) (.getTime)))

(defn to-hex [b]
  (.join (.map (js/Array.from (.slice b)) #(.slice (str "0" (.toString (bit-and % 0xff) 16)) -2)) ""))

(defn from-hex [b]
  (js/Uint8Array. (map #(js/parseInt (apply str %) 16) (partition 2 b))))

(defn fingerprint [x]
  (-> x
      (to-hex)
      (.substring 0 8)))

(defn pk-hex [state-val]
  (-> state-val
      :keypair
      (aget "publicKey")
      (to-hex)))

(defn median [& ar]
  (let [l (count ar)]
    (when (> l 0)
      (nth (sort ar) (int (/ l 2))))))

;; -------------------------
;; Crypto

(defn hash-object [t]
  (-> t
      (clj->js)
      (bencode/encode)
      (js/Uint8Array.from)
      (nacl.hash)))

(defn make-keypair []
  (nacl.sign.keyPair))

(defn sign-datastructure [keypair datastructure]
  (nacl.sign.detached
    (hash-object datastructure)
    (aget keypair "secretKey")))

(defn check-datastructure-signature [pk datastructure]
  (let [signature (datastructure :signature)
        datastructure (dissoc datastructure :signature)]
    (nacl.sign.detached.verify
      (hash-object datastructure)
      signature
      pk)))

(defn make-nonce []
  (nacl.randomBytes 8))

;; -------------------------
;; Blockchain

(def genesis-hash
  (hash-object {:seed "cljs-blockchain-ftw"}))

(defn compute-block-hash [t transactions previous-hash nonce]
  (hash-object [t transactions previous-hash nonce]))

(defn make-block [t transactions previous-hash new-index nonce]
  {:timestamp t
   :transactions transactions
   :hash (compute-block-hash t transactions previous-hash nonce)
   :nonce nonce
   :index new-index
   :previous-hash previous-hash})

(defn make-genesis-block []
  (make-block 0 #{} genesis-hash 0 (js/Uint8Array.from [0 0 0 0 0 0 0 0])))

(defn make-transaction [keypair to from amount fee]
  (let [transaction {:to (from-hex to)
                     :from (from-hex from)
                     :amount (int amount)
                     :fee (int fee)
                     :nonce (make-nonce)}
        signature (sign-datastructure keypair transaction)]
    (assoc transaction :signature signature)))

(defn compute-balance [transactions address]
  (let [ledger (for [transaction transactions]
                 (cond
                   (and (= (to-hex (transaction :from)) (to-hex (transaction :to)))
                        (= (to-hex (transaction :from)) address)) (* -1 (transaction :fee))
                   (= (to-hex (transaction :from)) address) (* -1 (+ (transaction :amount) (transaction :fee)))
                   (= (to-hex (transaction :to)) address) (transaction :amount)
                   :else 0))]
    (apply + ledger)))

(defn blockchain-transactions [blockchain]
  (for [block blockchain transaction (block :transactions)] transaction))

(defn is-genesis-block [block]
  (= (to-hex (hash-object block)) (to-hex (hash-object (make-genesis-block)))))

(defn is-valid-transaction [transactions transaction]
  (and
    (= (.-length (transaction :to)) 32)
    (= (.-length (transaction :from)) 32)
    (> (transaction :amount) 0)
    (<= (+ (transaction :amount) (transaction :fee)) (compute-balance transactions (to-hex (transaction :from))))
    (check-datastructure-signature (transaction :from) transaction)))

(defn is-valid-coinbase-transaction [transaction block]
  (and
    (= (transaction :fee) 0)
    (= (to-hex (transaction :from)) coinbase-from)
    (= (transaction :amount) (+ (apply + (map :fee (block :transactions))) block-reward))))

(defn is-valid-block [block blockchain]
  (let [previous-block (last blockchain)
        coinbase-transaction (first (block :transactions))
        transactions (rest (block :transactions))]
    (or
      (is-genesis-block block)
      (and
        (= (block :index) (inc (previous-block :index)))
        (= (to-hex (block :hash)) (to-hex (compute-block-hash (block :timestamp) (block :transactions) (previous-block :hash) (block :nonce))))
        (= (aget (block :hash) 0) 0)
        (= (count (remove #(is-valid-transaction (blockchain-transactions blockchain) %) transactions)) 0)
        (is-valid-coinbase-transaction coinbase-transaction block)))))

(defn is-valid-blockchain [blockchain]
  (let [invalid-blocks (remove identity
                               (for [p (range (count blockchain))]
                                 (let [[subchain remainder] (split-at p blockchain)]
                                   (is-valid-block (first remainder) subchain))))]
    (and
      (is-genesis-block (first blockchain))
      (= (count invalid-blocks) 0))))

(defn remove-blockchain-transactions-from-mempool [state-val]
  (update-in state-val [:mempool]
             (clojure.set/difference (set (blockchain-transactions (state-val :blockchain))))))

(defn add-block-to-blockchain [state-val new-block]
  (if (is-valid-block new-block (state-val :blockchain))
    (-> state-val
        (update-in [:blockchain] conj new-block)
        (remove-blockchain-transactions-from-mempool))
    state-val))

(defn add-transaction-to-mempool [state-val transaction]
  (if (is-valid-transaction (concat (blockchain-transactions (state-val :blockchain)) (state-val :mempool)) transaction)
    (update-in state-val [:mempool] conj transaction)
    state-val))

(defn mine-block [state-val]
  ; TODO: move vars inside the loop and use atom
  ; which could receive new transactions while we're mining
  (let [mempool-by-fee (reverse (sort-by :fee (state-val :mempool)))
        ; split top transactions by fee off mempool
        [transactions mempool-remaining] (split-at 9 mempool-by-fee)
        fees (apply + (map :fee transactions))
        coinbase-transaction (make-transaction (state-val :keypair) (pk-hex state-val) coinbase-from (+ block-reward fees) 0)
        transactions (conj transactions coinbase-transaction)
        previous-hash (-> state-val :blockchain (last) :hash)
        new-index (-> state-val :blockchain (count))]
    (loop [c 0]
      (let [candidate-block (make-block (now) transactions previous-hash new-index (make-nonce))]
        ; (js/console.log "candidate-block" c (clj->js candidate-block))
        ; find a block with one byte of leading zeros (fixed difficulty)
        (if (not= (aget (candidate-block :hash) 0) 0)
          (recur (inc c))
          candidate-block)))))

(defn resolve-blockchain-conflict [state-val new-blockchain]
  (if (and
        (> (count new-blockchain) (count (state-val :blockchain)))
        (is-valid-blockchain new-blockchain))
    (-> state-val
        (assoc :blockchain new-blockchain)
        (remove-blockchain-transactions-from-mempool))
    state-val))

(defn fee-calc [state-val f default]
  (or
    (apply f (map :fee (state-val :mempool)))
    default))

;; -------------------------
;; "network" - simulated with localStorage events
; no mutation before this point
; search "swap!" and "reset!" to find mutation points

(defonce storage (aget js/window "localStorage"))

(defn serializer [k v]
  (cond
    (= (type v) js/Uint8Array) (str "0x" (to-hex v))
    :else v))

(defn deserializer [k v]
  (cond
    (and (= (type v) js/String) (.substr v 0 2)) (from-hex (.substr v 2))
    :else v))

(defn send-message [kind message]
  (.setItem storage kind (-> message clj->js (js/JSON.stringify serializer)))
  (.setItem storage kind nil))

(defn receive [state ev]
  (let [kind (aget ev "key")
        message (-> (aget ev "newValue") (js/JSON.parse deserializer) (js->clj :keywordize-keys true))]
    (when message
      (print "got:" kind)
      (case kind
        "blockchain" (swap! state resolve-blockchain-conflict message)
        "transaction" (swap! state add-transaction-to-mempool message)))))

;; -------------------------
;; User interface

(defn submit-transaction! [state interface]
  (let [transaction (make-transaction
                      (@state :keypair)
                      (@interface :to)
                      (pk-hex @state)
                      (int (@interface :amount))
                      (int (@interface :fee)))]
    (swap! state add-transaction-to-mempool transaction)
    (send-message "transaction" transaction))
  (reset! interface {:fee (fee-calc @state median nil)}))

(defn submit-block! [state miner-ui]
  (reset! miner-ui "Mining a block...")
  (let [new-block (mine-block @state)]
    (swap! state add-block-to-blockchain new-block))
  (send-message "blockchain" (@state :blockchain))
  (reset! miner-ui (str "Found block: " (-> @state :blockchain last :hash (fingerprint)))))

(defn copy-pk []
  (let [pk-el (js/document.getElementById "pk")]
    (.focus pk-el)
    (.select pk-el)
    (.execCommand js/document "copy")))

(defn home-page [state]
  (let [interface (r/atom {})
        to (r/cursor interface [:to])
        amount (r/cursor interface [:amount])
        fee (r/cursor interface [:fee])
        miner-ui (r/cursor interface [:miner])]
    (fn []
      (let [balance (compute-balance (concat (blockchain-transactions (@state :blockchain)) (@state :mempool)) (pk-hex @state))]
        [:div
         [:div#header
          [:h2 "cljs-blockchain"]
          [:p [:a {:href "" :target "_blank"} "open more tabs to simulate network peers"] "."]]
         [:div#user
          [:h3 "wallet"]
          [:p "public key: " [:input#pk {:value (pk-hex @state) :readOnly true}] [:button {:on-click copy-pk} "copy"]]
          [:p "balance: " balance]]
         [:div#mining
          [:h3 "mining"]
          [:button {:on-click (partial submit-block! state miner-ui)} "mine a block"]
          [:span#mining @miner-ui]]
         [:div#ui
          [:h3 "make transaction"]
          [:input {:placeholder "to public key" :on-change #(reset! to (.replace (-> % .-target .-value) #"[^A-Fa-f0-9]" "")) :value @to}]
          [:input {:placeholder "amount" :on-change #(reset! amount (.replace (-> % .-target .-value) #"[^0-9]" "")) :value @amount}]
          [:input {:placeholder "fee" :on-change #(reset! fee (.replace (-> % .-target .-value) #"[^0-9]" "")) :value (or @fee (fee-calc @state median nil))}]
          [:button {:on-click (partial submit-transaction! state interface) :disabled (not (and (> (int @amount) 0) (<= (+ (int @amount) (int @fee)) balance) (= (.-length (from-hex @to)) 32)))} "Send"]]
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
                  [:div (fingerprint (t :from)) " -> " (fingerprint (t :to))
                   [:div.transaction-details
                    [:span.amount (t :amount)]
                    [:span.fee "fee: " (t :fee)]
                    [:span.signature "sig: " (fingerprint (t :signature))]]]]))])]
         [:p [:a#source {:href "https://github.com/chr15m/cljs-blockchain/blob/master/src/cljs_blockchain/core.cljs"} "view the source code"]]
         [:p [:a#resume {:href "https://mccormickit.com/resume"} "hire me"]]
         [:img#logo {:src "logo.svg"}]]))))

;; -------------------------
;; Initialize app

(defonce state (r/atom {:keypair (make-keypair)
                       :blockchain [(make-genesis-block)]
                       :mempool #{}}))

(defn mount-root []
  (r/render [home-page state] (.getElementById js/document "app")))

(defn init! []
  (.addEventListener js/window "storage" (partial receive state) false)
  (mount-root))

