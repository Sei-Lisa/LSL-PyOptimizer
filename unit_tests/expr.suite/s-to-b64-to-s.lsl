[ llStringToBase64("𝄞Áañ# +")
, llStringToBase64("")
, llBase64ToString("8J2EnsOBYcOxIyAr")
, llBase64ToString("")
, llBase64ToString("1")
, llBase64ToString("12")
, llBase64ToString("14A")
// Embedded and trailing NUL tests
, llBase64ToString("QUJDAERFRg") // 'ABC\x00DEF'
, llBase64ToString("AEEAQgBD") // '\x00A\x00B\x00C'
, llBase64ToString("AEEAQgBDAA") // '\x00A\x00B\x00C\x00'
, llBase64ToString("AEEAQgBDAAA=") // '\x00A\x00B\x00C\x00\x00'
// Miscellaneous tests
, llBase64ToString("gIAA")
, llBase64ToString("gAA")
, llBase64ToString("44AA")
, llBase64ToString("4IAh")
, llBase64ToString("gICAgGE")
, llBase64ToString("QQA")
, llBase64ToString("AEE=")
, llBase64ToString("wKE")
, llBase64ToString("9ICA")
, llBase64ToString("94CAgICA")
, llBase64ToString("4ICA")
, llBase64ToString("4IA")
, llUnescapeURL("%E0%80") // compare the result with the above's (extra "?")

// Invalid characters tests
, llBase64ToString("w4GA44HDgEFCQ9M") // C3 81 80 E3 81 C3 80 41 42 43 D3
, llBase64ToString("w4GA44HDgEFCQwDT") // C3 81 80 E3 81 C3 80 41 42 43 00 D3
, llBase64ToString("4ICAgICAgOOBw4BBQkMA0w") // E0 80 80 80 80 80 80 E3 81 C3 80 41 42 43 00 D3

// Test all UTF-8 ranges
// Normal ASCII range
, llBase64ToString("AHg") // 00 78
, llBase64ToString("AXg") // 01 78
, llBase64ToString("f3g") // 7F 78
, llBase64ToString("AQIDBAUGBwgJCgsMDQ4P") // 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F
, llBase64ToString("EBESExQVFhcYGRobHB0eHw") // 10 11 12 13 14 15 16 17 18 19 1A 1B 1C 1D 1E 1F

// Invalid range (characters used for 2nd position and on)
, llBase64ToString("gIB4") // 80 80 78
, llBase64ToString("v794") // BF BF 78
// Aliased range U+0000 - U+007F
, llBase64ToString("wIB4") // C0 80 78
, llBase64ToString("wb94") // C1 BF 78
// Valid U+0080 - U+07FF
, llBase64ToString("woB4") // C2 80 78
, llBase64ToString("3794") // DF BF 78
// Aliased range U+0000 - U+07FF
, llBase64ToString("4ICAeA") // E0 80 80 78
, llBase64ToString("4J+/eA") // E0 9F BF 78
// Valid U+0800 - U+D7FF
, llBase64ToString("4KCAeA") // E0 A0 80 78
, llBase64ToString("7Z+/eA") // ED 9F BF 78
// UTF-16 surrogates area
// High surrogates U+D800 - U+DBFF
, llBase64ToString("7aCAeA") // ED A0 80 78
, llBase64ToString("7a+/eA") // ED AF BF 78
// Low surrogates U+DC00 - U+DFFF
, llBase64ToString("7bCAeA") // ED B0 80 78
, llBase64ToString("7b+/eA") // ED BF BF 78
// Valid U+E000 - U+FFFF
, llBase64ToString("7oCAeA") // EE 80 80 78
, llBase64ToString("77+/eA") // EF BF BF 78
// Aliased range U+0000 - U+FFFF
, llBase64ToString("8ICAgHg") // F0 80 80 80 78
, llBase64ToString("8I+/v3g") // F0 8F BF BF 78
// Valid range U+10000 - U+10FFFF
, llBase64ToString("8JCAgHg") // F0 90 80 80 78
, llBase64ToString("9I+/v3g") // F4 8F BF BF 78
// Out of range U+110000 - U + 1FFFFF
, llBase64ToString("9JCAgHg") // F4 90 80 80 78
, llBase64ToString("97+/v3g") // F7 BF BF BF 78
// Aliased range U+0000 - U+1FFFFF
, llBase64ToString("+ICAgIB4") // F8 80 80 80 80 78
, llBase64ToString("+Ie/v794") // F8 87 BF BF BF 78
// Out of range U+200000 - U+3FFFFFF
, llBase64ToString("+IiAgIB4") // F8 88 80 80 80 78
, llBase64ToString("+7+/v794") // FB BF BF BF BF 78
// Aliased range U+0000 - U+3FFFFFF
, llBase64ToString("/ICAgICAeA") // FC 80 80 80 80 80 78
, llBase64ToString("/IO/v7+/eA") // FC 83 BF BF BF BF 78
// Out of range U+4000000 - U+7FFFFFFF
, llBase64ToString("/ISAgICAeA") // FC 84 80 80 80 80 78
, llBase64ToString("/b+/v7+/eA") // FD BF BF BF BF BF 78
// Invalid in UTF-8 (used for UTF-16 BOM)
, llBase64ToString("/rCAgICAgIB4") // FE B0 80 80 80 80 80 80 78
, llBase64ToString("/7+/v7+/v794") // FF BF BF BF BF BF BF BF 78
// Short or invalid sequences
, llBase64ToString("gA") // 80
, llBase64ToString("vw") // BF
, llBase64ToString("wg") // C2
, llBase64ToString("4Q") // E1
, llBase64ToString("4YA") // E1 80
, llBase64ToString("8Q") // F1
, llBase64ToString("8YA") // F1 80
, llBase64ToString("8YCA") // F1 80 80
, llBase64ToString("+ICAgA") // F8 80 80 80
, llBase64ToString("+IiAgA") // F8 88 80 80
, llBase64ToString("/ICAgIA") // FC 80 80 80 80
, llBase64ToString("/ISAgIA") // FC 84 80 80 80
, llBase64ToString("77+9kA") // EF BF BD 90
]
