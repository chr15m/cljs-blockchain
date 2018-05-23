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

;*** blockchain ***;

(defn blockchain-make-genesis-block [t]
  {:index 1
   :timestamp 0
   :transactions #{}
   :epoch (compute-epoch t)
   ; one reason why you should not use this for anything serious:
   ; somebody can generate an entire epoch worth of blocks
   ; for some future block, winning all mining rewards for the block
   ; because the following value is deterministic and predictable
   :pow (nacl.hash (js/Uint8Array. (str "cljs-blockchain #" (compute-epoch t))))
   :previous-hash 0x1})

;*** data manipulation ***;

(defn to-hex [b]
  (.join (.map (js/Array.from (.slice b)) #(.slice (str "0" (.toString (bit-and % 0xff) 16)) -2)) ""))

(defn from-hex [b]
  (js/Uint8Array. (map #(js/parseInt (apply str %) 16) (partition 2 b))))

(defn pk [state]
  (-> state
      :keypair
      (or #js {})
      (.. -publicKey)
      (or #js [])
      (to-hex)))

;*** crypto ***;

(defn ensure-keypair! []
  (let [k (cljs.reader/read-string (.getItem storage "secret-key"))
        k (if k (nacl.sign.keyPair.fromSecretKey (js/Uint8Array. k)) (nacl.sign.keyPair))]
    (.setItem storage "secret-key" (prn-str (js/Array.from (.-secretKey k))))
    k))

;*** main event loop ***;

(defn add-transaction-to-mempool [new-state {:keys [to from amount]}]
  (print "adding transaction:" to from amount)
  ; TODO: regex check to is hex
  ; TODO: regex check from is hex
  ; TODO: regex check amount is number
  (let [to (from-hex to)
        from (from-hex from)
        amount (int amount)]
    ; TODO: also check balance
    ; TODO: also check from address is own
    (if (and
          (= (.-length to) 32)
          (= (.-length from) 32)
          (> amount 0))
      (update-in new-state [:mempool] conj {:to to :from from :amount amount})
      new-state)))

(defn process-event [new-state event]
  (cond
    (event :add-transaction) (add-transaction-to-mempool new-state (event :add-transaction))
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
          new-state (process-event new-state event)]
      (js/console.log "main-loop new-state:" (clj->js new-state))
      new-state)))

(defn check-epoch [state]
  (when (has-epoch-changed (compute-epoch (now)) (@state :blockchain))
    (put! (@state :incoming) :update-epoch)))

;; -------------------------
;; Views

(defn home-page [state]
  (let [to (r/atom "")
        amount (r/atom "")]
    (fn []
      [:div
       [:h2 "cljs-blockchain"]
       [:small "provided 'as-is' without warranty of any kind. " [:strong "this is a toy"] "."]
       [:p "this blockchain resets every 24 hrs, deleting all history."]
       [:h3 "epoch: " (get-in @state [:blockchain 0 :epoch])]
       [:div "Your public key:" [:pre (pk @state)]]
       [:div [:h3 "Add a transaction:"]
        [:input {:placeholder "to public key" :on-change #(reset! to (-> % .-target .-value)) :value @to}]
        [:input {:placeholder "amount" :on-change #(reset! amount (-> % .-target .-value)) :value @amount}]
        ; [:input {:placeholder "message"}]
        [:button {:on-click #(put! (@state :incoming) {:add-transaction {:to @to :from (pk @state) :amount (int @amount)}})} "Send"]]])))

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

