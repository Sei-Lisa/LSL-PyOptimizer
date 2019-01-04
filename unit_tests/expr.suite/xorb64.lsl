[ llXorBase64("𒍅", "")
, llXorBase64("", "ABCD")
, llXorBase64("", "?")
, llXorBase64("AB", "?")
, llXorBase64("AABA", "1234")
, llXorBase64("1234", "AABA")
, llXorBase64("BAAA", "1234")
, llXorBase64("1234", "BAAA")
, llXorBase64("AABA", "AABA")
, llXorBase64("AABA", "AABC")
, llXorBase64("AABC", "AABA")
, llXorBase64("Hello, World!", "XYZXYZ")
, llXorBase64("QG8y", "XYZXYZ")
, llXorBase64("ABCDABCDABCD", "ABCD") // Vulnerable to BUG-3763
, llXorBase64("ABCDABCDABCDABCDABCDABCDABCD", "ABCD") // Vulnerable to BUG-3763
, llXorBase64("ABCDABCDABCD", "ABC=") // Vulnerable to BUG-3763
, llXorBase64("Stuffs not b64 <^.^>!", "AA==")
, llXorBase64("ABCDABCDABCD", "Stuffs not b64 <^.^>!")
, llXorBase64("AQCDAQCD", "AQC=")
]
