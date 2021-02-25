from darrow import HexConverter

def test_conversion():
	hex_string = "45766964696e74"
	expected = "RXZpZGludA=="
	converter = HexConverter()
	assert converter.convert(hex_string) == expected

