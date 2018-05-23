(ns cljs-blockchain.core
  (:require
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

(defn now []
  (-> (js/Date.) (.getTime)))

(defn compute-epoch [t]
  (-> t (/ ms-per-day) (int)))

(defn until-next-epoch [])

;*** blockchain ***;

(defn blockchain-make-genesis-block [t]
  {:index 1
   :timestamp 0
   :transactions #{}
   :epoch (compute-epoch t)
   :proof 0x1
   :previous-hash 0x1})

(defn blockchain-init []
  {:transactions #{}
   :chain []})

(defn has-epoch-changed [epoch blockchain]
  (not= epoch (get-in blockchain [:chain 0 :epoch])))

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

;*** main event loop ***;

(defn main-loop [old-state]
  (go
    (let [new-state old-state
          ; wait for events
          event (<! (new-state :incoming))
          ; compute current epoch
          current-epoch (compute-epoch (now))
          ; ensure wallet keys
          new-state (update-in new-state [:keypair] #(or % (nacl.sign.keyPair)))
          ; ensure webtorrent instance
          ;new-state (update-in new-state [:net] #(or % (wt.)))
          ; ensure blockchain structure
          new-state (update-in new-state [:blockchain] #(if (has-epoch-changed current-epoch %) (blockchain-init) %))
          ; ensure genesis block
          new-state (update-in new-state [:blockchain :chain 0] #(or % (blockchain-make-genesis-block (now))))
          ; ensure mempool structure
          new-state (update-in new-state [:mempool] #(or % #{}))
          ; ensure webtorrent connection
          ]
      (js/console.log "main-loop new-state:" (clj->js new-state))
      new-state)))

(defn check-epoch [state]
  (when (has-epoch-changed (compute-epoch (now)) (@state :blockchain))
    (put! (@state :incoming) :update-epoch)))

;; -------------------------
;; Views

(defn home-page [state]
  [:div
   [:small "provided 'as-is' without warranty of any kind. " [:strong "this is a toy"] "."]
   [:h2 "cljs-blockchain"]
   [:h3 "epoch: " (get-in @state [:blockchain :chain 0 :epoch])]
   [:div "Your public key:" [:pre (pk @state)]]
   [:div [:h3 "Add a transaction:"]
    [:input {:placeholder "to public key"}]
    [:input {:placeholder "amount"}]
    [:input {:placeholder "message"}]
    [:button "Send"]]])

;; -------------------------
;; Initialize app

(defn mount-root []
  (put! (@state :incoming) :init)
  (r/render [home-page state] (.getElementById js/document "app")))

(defn init! []
  ; app state mutation happens here
  (go-loop [] (reset! state (<! (main-loop @state))) (recur))
  ; check for blockchain epoch change (reset chain daily) every second
  (js/setInterval #(check-epoch state) 1000)
  (mount-root))
