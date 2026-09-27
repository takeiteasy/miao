(in-package #:miao/tests)
(in-suite :miao)

(test system-loads
  (is (find-package '#:miao))
  (is (stringp miao:*version*)))

(test meow-is-available
  (is (find-package '#:meow))
  (is (fboundp 'meow:start-service)))

(test json-round-trip
  ;; Compare values, not the serialised string: jzon parses into a hash table
  ;; and key order in STRINGIFY follows the implementation's hash iteration.
  (let* ((src "{\"a\":1,\"b\":[true,false,null],\"c\":{\"d\":\"x\"}}")
         (v (com.inuoe.jzon:parse src))
         (w (com.inuoe.jzon:parse (com.inuoe.jzon:stringify v))))
    (is (= 1 (gethash "a" w)))
    (is (equal "x" (gethash "d" (gethash "c" w))))
    (is (equalp (gethash "b" v) (gethash "b" w)))
    (is (eq 'cl:null (aref (gethash "b" w) 2)))
    (is (null (aref (gethash "b" w) 1)))))

(test json-reads-doubles
  ;; Numbers outside single-float range must survive; the adapters rely on it.
  (let ((v (com.inuoe.jzon:parse "{\"big\":1e300,\"tiny\":1e-7,\"i\":7}")))
    (is (typep (gethash "big" v) 'double-float))
    (is (typep (gethash "tiny" v) 'double-float))
    (is (integerp (gethash "i" v)))))

(test http-client-is-available
  (is (find-package '#:drakma))
  (is (fboundp 'drakma:http-request))
  (is (string= "a%2Fb" (drakma:url-encode "a/b" :utf-8))))

(test http-live-request
  ;; Off by default: CI must not depend on the network.
  (if (uiop:getenv "MIAO_LIVE_HTTP")
      (multiple-value-bind (body status)
          (drakma:http-request "https://httpbingo.org/status/404" :redirect nil)
        (declare (ignore body))
        (is (= 404 status)))
      (skip "set MIAO_LIVE_HTTP to run live HTTP tests")))
