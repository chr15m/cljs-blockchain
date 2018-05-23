(ns ^:figwheel-no-load cljs-blockchain.dev
  (:require
    [cljs-blockchain.core :as core]
    [devtools.core :as devtools]))


(enable-console-print!)

(devtools/install!)

(core/init!)
