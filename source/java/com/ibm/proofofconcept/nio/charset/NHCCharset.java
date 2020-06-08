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

	char aBig5Char = '\uA470';// <--big5--> 小
	char bBig5Char = '\uA7B5';// <--big5--> 孝
	char aUnicodeChar = '\u5C0F';// <--unicode 小
	char bUnicodeChar = '\u5B5D';// <--unicode 孝
	Character[] big5Chars = new Character[] { '\uA470', // <--big5--> 小
			'\uCE5D'// <--big5--> 垚
	};
	Character[] fbig5Chars = new Character[] { '\uA470', // <--big5--> 小
			'\uA7B5'// <--big5--> facked 垚
	};
	Character[] unicodeChars = new Character[] { '\u5C0F', // <--big5--> 小
			'\u5B5D'// <--big5--> facked 垚 big5-->A7B5 孝
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
			CoderResult cr;

			do {
				while (cb.hasRemaining()) {
					// Peek at the character at the current input position
					char inputChar = cb.charAt(0);
					for (int i = 0; i < unicodeChars.length; i++) {
						if (unicodeChars[i] == inputChar) {
							// Consume this character - i.e. move the position
							inputChar = cb.get();
							bb.putChar(big5Chars[i]);
							break;
						}
					}
				}

				// This encoder can't encode the character at this position,
				// so get the superclass to do it
				// Alternatively, we've gotten to the end of this buffer
				baseEncoder.reset();

				cr = baseEncoder.encode(cb, bb, cb.hasRemaining());
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

			do {
				while (bb.remaining() > 1) {
					// Peek at the character at the current input position
					char inputChar = bb.getChar();
					bb.mark();
					int binarySearch = Arrays.binarySearch(big5Chars, inputChar);
					if (binarySearch > 0) {
						cb.put(unicodeChars[binarySearch]);
						bb.mark();
					} else {
						// We can't decode this character, so give up and let
						// the super-class do it
						bb.reset();
						break;
					}
				}

				baseDecoder.reset();

				cr = baseDecoder.decode(bb, cb, bb.hasRemaining());
			} while (cr.isUnmappable());
			return (cr);
		}
	}

	public static byte[] charToByte(char c) {
		byte[] b = new byte[2];
		b[0] = (byte) ((c & 0xFF00) >> 8);
		b[1] = (byte) (c & 0xFF);
		return b;
	}
}
