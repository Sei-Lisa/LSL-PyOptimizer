[ llUnescapeURL("%")
, llUnescapeURL("%%")
, llUnescapeURL("%4%252Fabc")
, llEscapeURL(llUnescapeURL("%%4%252Fabc"))
, llStringToBase64(llUnescapeURL("%%4%252Fabc"))

, llEscapeURL(llUnescapeURL("%.44%25%%2Fa←c"))
, llEscapeURL(llUnescapeURL("%.44%25%%2Fa←c%"))
, llEscapeURL(llUnescapeURL("%.44%25%%2Fa←c%2"))
, llEscapeURL(llUnescapeURL("%.44%25%%2Fa←c%%"))
, llEscapeURL(llUnescapeURL("%.44%25%%2Fa←c%%2"))
, llEscapeURL(llUnescapeURL("%.44%25%%2Fa←c%%%2346"))
, llEscapeURL(llUnescapeURL("%4.%25"))

// Test UTF-8 validity
, llEscapeURL(llUnescapeURL("%C3%81%80%E3%81%C3%80ABC%D3"))
, llEscapeURL(llUnescapeURL("%C3%81%80%E3%81%C3%80ABC%00%D3"))
, llEscapeURL(llUnescapeURL("%E0%80%80%80%80%80%80%E3%81%C3%80ABC%00%D3"))
// test UTF-8 valid ranges
// thorough control codes
, llEscapeURL(llUnescapeURL("%01%02%03%04%05%06%07%08%09%0A%0B%0C%0D%0E%0F"))
, llEscapeURL(llUnescapeURL("%11%12%13%14%15%16%17%18%19%1A%1B%1C%1D%1E%1F"))
// DEL
, llEscapeURL(llUnescapeURL("%7Fx"))
// invalid ranges
, llEscapeURL(llUnescapeURL("%80%80x"))
, llEscapeURL(llUnescapeURL("%BF%BFx"))
// aliased
, llEscapeURL(llUnescapeURL("%C0%80x"))
, llEscapeURL(llUnescapeURL("%C1%BFx"))
// valid range
, llEscapeURL(llUnescapeURL("%C2%80x"))
, llEscapeURL(llUnescapeURL("%DF%BFx"))
// aliased
, llEscapeURL(llUnescapeURL("%E0%80%80x"))
, llEscapeURL(llUnescapeURL("%E0%9F%BFx"))
// valid range
, llEscapeURL(llUnescapeURL("%E0%A0%80x"))
, llEscapeURL(llUnescapeURL("%ED%9F%BFx"))
// invalid (UTF-16 high surrogates)
, llEscapeURL(llUnescapeURL("%ED%A0%80x"))
, llEscapeURL(llUnescapeURL("%ED%AF%BFx"))
// invalid (UTF-16 low surrogates)
, llEscapeURL(llUnescapeURL("%ED%B0%80x"))
, llEscapeURL(llUnescapeURL("%ED%BF%BFx"))
// valid range
, llEscapeURL(llUnescapeURL("%EE%80%80x"))
, llEscapeURL(llUnescapeURL("%EF%BF%BFx"))
// aliased
, llEscapeURL(llUnescapeURL("%F0%80%80%80x"))
, llEscapeURL(llUnescapeURL("%F0%8F%BF%BFx"))
// valid range
, llEscapeURL(llUnescapeURL("%F0%90%80%80x"))
, llEscapeURL(llUnescapeURL("%F4%8F%BF%BFx"))
// codepoints > U+10FFFF (all invalid)
, llEscapeURL(llUnescapeURL("%F4%90%80%80x"))
, llEscapeURL(llUnescapeURL("%F7%BF%BF%BFx"))
, llEscapeURL(llUnescapeURL("%F8%80%80%80%80x"))
, llEscapeURL(llUnescapeURL("%F8%87%BF%BF%BFx"))
, llEscapeURL(llUnescapeURL("%F8%88%80%80%80x"))
, llEscapeURL(llUnescapeURL("%FB%BF%BF%BF%BFx"))
, llEscapeURL(llUnescapeURL("%FC%80%80%80%80%80x"))
, llEscapeURL(llUnescapeURL("%FC%83%BF%BF%BF%BFx"))
, llEscapeURL(llUnescapeURL("%FC%84%80%80%80%80x"))
, llEscapeURL(llUnescapeURL("%FD%BF%BF%BF%BF%BFx"))
// FE and FF are used for the UTF-16 and UTF-32 BOM
, llEscapeURL(llUnescapeURL("%FE%B0%80%80%80%80%80%80x"))
, llEscapeURL(llUnescapeURL("%FF%BF%BF%BF%BF%BF%BF%BFx"))
// short or invalid sequences
, llEscapeURL(llUnescapeURL("%80"))
, llEscapeURL(llUnescapeURL("%BF"))
, llEscapeURL(llUnescapeURL("%C2"))
, llEscapeURL(llUnescapeURL("%E1"))
, llEscapeURL(llUnescapeURL("%E1%80"))
, llEscapeURL(llUnescapeURL("%F1"))
, llEscapeURL(llUnescapeURL("%F1%80"))
, llEscapeURL(llUnescapeURL("%F1%80%80"))

// Test that U+FFFD is preserved even with invalid characters
, llEscapeURL(llUnescapeURL("%EF%BF%BD%90"))

]
