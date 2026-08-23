(ns clj-protobuf.core
  "Serialization, on top of what the generated code produces.

  Generated `X->proto` / `proto->X` fns are Message-in/Message-out — they never
  touch bytes. This namespace is the other half: Message to bytes and back.

  Typical round trip:

      (-> rec HelloRequest->proto pb/encode)                     ; -> bytes
      (->> bytes (pb/decode HelloRequest-prototype) proto->HelloRequest)

  `decode` takes the generated `X-prototype` var — the same value everything
  else in the contract keys off — so there is exactly one handle per message
  type in user code."
  (:import [com.google.protobuf
            ByteString
            InvalidProtocolBufferException
            Message
            Parser]
           [java.io InputStream OutputStream]
           [java.nio ByteBuffer]))

(set! *warn-on-reflection* true)

(defn encode
  "Message -> bytes. With an OutputStream, writes to it and returns it."
  (^bytes [^Message msg] (.toByteArray msg))
  ([^Message msg ^OutputStream out] (.writeTo msg out) out))

(defn encode-delimited
  "Write msg length-prefixed, for framed streams of messages."
  [^Message msg ^OutputStream out]
  (.writeDelimitedTo msg out)
  out)

(defn- parse-error [^Throwable e]
  (ex-info "failed to parse protobuf message"
           {:clj-protobuf/error :parse}
           e))

(defn decode
  "Parse one message. `prototype` is the generated `X-prototype` var; `input`
  is a byte array, ByteString, ByteBuffer, or InputStream (read to the end)."
  (^Message [prototype input]
   (let [parser ^Parser (.getParserForType ^Message prototype)]
     (try
       (cond
         (bytes? input)                    (.parseFrom parser ^bytes input)
         (instance? ByteString input)      (.parseFrom parser ^ByteString input)
         (instance? ByteBuffer input)      (.parseFrom parser ^ByteBuffer input)
         (instance? InputStream input)     (.parseFrom parser ^InputStream input)
         :else (throw (ex-info (str "cannot decode from " (some-> input class (.getName)))
                               {:clj-protobuf/error :parse
                                :input-class (some-> input class (.getName))})))
       (catch InvalidProtocolBufferException e
         (throw (parse-error e)))))))

(defn decode-delimited
  "Parse one length-prefixed message from a stream; nil at end of stream."
  ^Message [prototype ^InputStream in]
  (try
    (.parseDelimitedFrom ^Parser (.getParserForType ^Message prototype) in)
    (catch InvalidProtocolBufferException e
      (throw (parse-error e)))))

(defn unknown-fields
  "The unknown fields protobuf-java preserved on a parsed Message. Preserved on
  the Message; necessarily dropped by a record round trip (`proto->X` reads
  declared fields only), so re-encoding a record is not byte-identical to the
  original when unknown fields were present. Inspect them here when that
  matters."
  [^Message msg]
  (.getUnknownFields msg))
