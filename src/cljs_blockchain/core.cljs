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

(defonce state (r/atom {}))

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
    (js/console.log "main-loop old-state:" (clj->js old-state))
    (let [new-state old-state
          ; ensure incoming channel
          new-state (update-in new-state [:incoming] #(or % (chan)))
          ; ensure wallet keys
          new-state (update-in new-state [:keypair] #(or % (nacl.sign.keyPair)))
          ; ensure webtorrent instance
          ;new-state (update-in new-state [:net] #(or % (wt.)))
          ; ensure blockchain structure
          ; ensure webtorrent connection
          ; wait for events
          event (<! (new-state :incoming))]
      (js/console.log "event:" (clj->js event))
      new-state)))

;; -------------------------
;; Views

(defn home-page []
  [:div [:h2 "Welcome to Reagent"]])

;; -------------------------
;; Initialize app

(defn mount-root []
  ;(put! (@state :incoming) :init)
  (r/render [home-page state] (.getElementById js/document "app")))

(defn init! []
  (go-loop [] (reset! state (<! (main-loop @state))) (recur))
  (mount-root))
