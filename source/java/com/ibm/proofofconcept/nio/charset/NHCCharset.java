package com.ibm.proofofconcept.nio.charset;

import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.Charset;
import java.nio.charset.CharsetDecoder;
import java.nio.charset.CharsetEncoder;
import java.nio.charset.CoderResult;
import java.nio.charset.CodingErrorAction;
import java.util.Arrays;

public class NHCCharset extends Charset {
	// The name of the base charset encoding we delegate to.
	// Since the NHC custom encoding is based on Big-5, delegate to Big-5
	private static final String BASE_CHARSET_NAME = "Big5";

	// Handle to the real charset we'll use for transcoding between
	// characters and bytes.
	Charset baseCharset;

	// Original HEX
	Character[] big5Chars = new Character[] { '\uCE5D'// <--big5--> 垚
	};
	// Target HEX
	Character[] fbig5Chars = new Character[] { '\uA7B5'// <--big5--> facked 垚
	};
	// Target HEX mapped unicode
	Character[] unicodeChars = new Character[] { '\u5B5D'// <--big5--> facked 垚 big5-->A7B5 孝
	};

	/**
	 * Constructor for the NHC charset. Call the superclass constructor to pass
	 * along the name(s) we'll be known by. Then save a reference to the delegate
	 * Charset.
	 */
	protected NHCCharset(String canonical, String[] aliases) {
		super(canonical, aliases);

		// Save the base charset we're delegating to.
		baseCharset = Charset.forName(BASE_CHARSET_NAME);

	}

	// ----------------------------------------------------------

	/**
	 * Called by users of this Charset to obtain an encoder. This implementation
	 * instantiates an instance of a private class (defined below) and passes it an
	 * encoder from the base Charset.
	 */
	public CharsetEncoder newEncoder() {
		return new NHCEncoder(this, baseCharset.newEncoder());
	}

	/**
	 * Called by users of this Charset to obtain a decoder. This implementation
	 * instantiates an instance of a private class (defined below) and passes it a
	 * decoder from the base Charset.
	 */
	public CharsetDecoder newDecoder() {
		return new NHCDecoder(this, baseCharset.newDecoder());
	}

	/**
	 * This method must be implemented by concrete Charsets. We always say no, which
	 * is safe.
	 */
	public boolean contains(Charset cs) {
		return (cs.equals(this));
	}

	/**
	 * The encoder implementation for the NHC Charset. This class, and the matching
	 * decoder class below, should also override the "impl" methods, such as
	 * implOnMalformedInput() and make passthrough calls to the baseEncoder object.
	 * That is left as an exercise for the hacker.
	 */
	private class NHCEncoder extends CharsetEncoder {
		private CharsetEncoder baseEncoder;

		/**
		 * Constructor, call the superclass constructor with the Charset object and the
		 * encodings sizes from the delegate encoder.
		 */
		NHCEncoder(Charset cs, CharsetEncoder baseEncoder) {
			super(cs, baseEncoder.averageBytesPerChar(), baseEncoder.maxBytesPerChar());

			// We want to override the behaviour on seeing unmappable character input so
			// that we can do the mapping ourselves
			baseEncoder.onUnmappableCharacter(CodingErrorAction.REPORT);

			this.baseEncoder = baseEncoder;
		}

		/**
		 * Implementation of the encoding loop.
		 */
		@Override
		protected CoderResult encodeLoop(CharBuffer cb, ByteBuffer bb) {
			CoderResult cr = null;
			Charset commonCharset = Charset.forName("big5");

			do {
				boolean ucdFlag = false;
				CharBuffer cbpForFlag = cb.duplicate();
				// Big5 result
				char cb1InputChar = cbpForFlag.get();
				for (int i = 0; i < unicodeChars.length; i++) {
					if (unicodeChars[i] == cb1InputChar) {
						ucdFlag = true;
						break;
					}
				}
				if (!ucdFlag) {
					// if doesn't includes 垚， use big5 result
					CharsetEncoder newEncoder = commonCharset.newEncoder();
					newEncoder.reset();
					cr = newEncoder.encode(cb, bb, bb.hasRemaining());
				} else {
					// Big5 result + replaced 垚
					CharBuffer cbp1 = cb.duplicate();
					ByteBuffer bb1 = commonCharset.encode(cbp1);
					CharBuffer cb1 = commonCharset.decode(bb1);
					while (cb.hasRemaining()) {
						// Peek at the character at the current input position
						char inputChar = cb.get();
						char cb1Char = cb1.get();
						boolean flag = false;
						for (int i = 0; i < unicodeChars.length; i++) {
							if (unicodeChars[i] == inputChar) {
								// Consume this character - i.e. move the position
								bb.putChar(big5Chars[i]);
								flag = true;
								break;
							}
						}
						if (!flag) {
							bb.putChar(cb1Char);
						}
					}
					cr = baseEncoder.encode(cb, bb, false);
				}
			} while (cr.isUnmappable());

			return (cr);
		}
	}

	// --------------------------------------------------------

	/**
	 * The decoder implementation for the NHC Charset.
	 */
	private class NHCDecoder extends CharsetDecoder {
		private CharsetDecoder baseDecoder;

		/**
		 * Constructor, call the superclass constructor with the Charset object and pass
		 * along the chars/byte values from the delegate decoder.
		 */
		NHCDecoder(Charset cs, CharsetDecoder baseDecoder) {
			super(cs, baseDecoder.averageCharsPerByte(), baseDecoder.maxCharsPerByte());

			// We want to override the behaviour on seeing unmappable character input so
			// that we can do the mapping ourselves
			baseDecoder.onUnmappableCharacter(CodingErrorAction.REPORT);
			this.baseDecoder = baseDecoder;
		}

		/**
		 * Implementation of the decoding loop.
		 */
		protected CoderResult decodeLoop(ByteBuffer bb, CharBuffer cb) {
			CoderResult cr;
			int previousPosition = -1;
			ByteBuffer bbdp1 = bb.duplicate();
			Charset commonCharset = Charset.forName("big5");
			// Big5 result
			CharBuffer s = commonCharset.decode(bbdp1);

			do {
				if (previousPosition > -1 && bb.position() == previousPosition) {
					// If the character is unmappable in either our extension or the base encoder,
					// then
					// throw away the character and replace it with the replacement character from
					// the
					// base encoder
					bb.get();
					cb.put(baseDecoder.replacement());
				}
				previousPosition = bb.position();

				bb.mark();
				int i = 0;
				while (bb.remaining() > 1) {
					// Peek at the character at the current input position
					char inputChar = bb.getChar();
					int binarySearch = Arrays.binarySearch(big5Chars, inputChar);
					if (binarySearch >= 0) {
						// replace specific char
						cb.put(unicodeChars[binarySearch]);
					} else {
						// get char from big5 result
						cb.put(s.get(i));
					}
					bb.mark();
					i++;
				}

				cr = baseDecoder.decode(bb, cb, false);
			} while (cr.isUnmappable());
			return (cr);
		}
	}

	public static Character decodeUnicode(String unicode) {
		if (!unicode.contains("\\u")) {
			return null;
		}
		int data = Integer.parseInt(unicode.replace("\\u", ""), 16);
		return (char) data;
	}

	public static byte[] charToByte(char c) {
		byte[] b = new byte[2];
		b[0] = (byte) ((c & 0xFF00) >> 8);
		b[1] = (byte) (c & 0xFF);
		return b;
	}
}
