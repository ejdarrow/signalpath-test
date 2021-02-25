import codecs


class HexConverter:
	def __init__(self):
		pass

	# This is the easy way, which I would do in production.
	def codecs(self, hex_string:str) -> str:
		return codecs.encode(codecs.decode(hex_string, 'hex'), 'base64').decode().replace("\n","")

	def convert(self, hex_string:str) -> str:
		return self.by_hand(hex_string)
		#return self.codecs(hex_string)

	# This is the academic way.
	def by_hand(self, hex_string:str) -> str:
		"""

		"""
		binary = bin(int(hex_string, 16))
		sixtyfour = self.make_it_base_64_from_binary(binary)
		
		return sixtyfour + "=="

	def make_it_base_64_from_binary(self, bits) -> str:
		i = 0
		pieces = []
		bitstring = bits.replace('0b', '0')
		sf_bytes = len(bitstring)//6
		if sf_bytes * 6 < len(bitstring): #Base64 Padding
			print("length mismatch.")
			sf_bytes += 1
			offset = sf_bytes * 6 - len(bitstring)
			bitstring += '0'*offset	
	
		chars = ""
		for i in range(sf_bytes):
			piece = bitstring[i * 6: (i + 1) * 6]
			pieces.append(piece)
			int_piece = int(piece, 2)
			if int_piece < 26:
				chars += chr(int_piece + 65)
			elif int_piece < 52:
				chars += chr(int_piece + 71)
			elif int_piece < 62:
				chars += str(int_piece - 52)
			elif int_piece == 62:
				chars += "+"
			elif int_piece == 63:
				chars += "/"
		return chars

