package com.ibm.proofofconcept.nio.charset;

import java.nio.charset.Charset;
import java.nio.charset.spi.CharsetProvider;
import java.util.HashSet;
import java.util.Iterator;

/**
 * A CharsetProvider class which makes available the charsets provided by this
 * package. Currently there is only one, namely the cp99999 charset. This is
 * not a registered IANA charset.
 * 
 * To activate this CharsetProvider, it's necessary to add a file to the
 * classpath of the JVM runtime at the following location:
 * META-INF/services/java.nio.charsets.spi.CharsetProvider
 * 
 * That file must contain a line with the fully qualified name of this class on
 * a line by itself: com.ibm.proofofconcept.nio.charset.NHCCharsetProvider
 * 
 * See the javadoc page for java.nio.charset.spi.CharsetProvider for full
 * details.
 * 
 */
public class NHCCharsetProvider extends CharsetProvider {
	// The name of the charset we provide
	public static final String NHC_CHARSET_NAME = "Cp99999";
	public static final String NHC_CHARSET_NO = "99999";

	// A handle to the Charset object
	private Charset nhc = null;

	/**
	 * Constructor, instantiate a Charset object and save the reference.
	 */
	public NHCCharsetProvider() {
		String[] alias = new String[] { NHC_CHARSET_NAME, NHC_CHARSET_NO};
		this.nhc = new NHCCharset(NHC_CHARSET_NAME, alias);
	}

	/**
	 * Called by Charset static methods to find a particular named Charset. If it's
	 * the name of this charset (we don't have any aliases) then return the NHC
	 * Charset, else return null.
	 */
	@Override
	public Charset charsetForName(String charsetName) {
		if (NHC_CHARSET_NAME.equals(charsetName)) {
			return (nhc);
		}

		return (null);
	}

	/**
	 * Return an Iterator over the set of Charset objects we provide.
	 * 
	 * @return An Iterator object containing references to all the Charset objects
	 *         provided by this class.
	 */
	@Override
	public Iterator<Charset> charsets() {
		HashSet<Charset> set = new HashSet<Charset>(1);

		set.add(nhc);

		return (set.iterator());
	}
}
