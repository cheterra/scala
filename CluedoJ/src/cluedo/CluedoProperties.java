package cluedo;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.Properties;

class CluedoProperties {
	
	Properties props;
	
	public CluedoProperties(String name) {
	  load(name);
  }

	private void load(String name) {
		try {
	    File fi = new File(name);
	    props = new Properties();
	    props.load(new FileReader(fi));
    } catch (IOException e) {
	    // TODO Auto-generated catch block
	    e.printStackTrace();
    }
	}
	
	public String get(String name) {
		return props.getProperty(name, "");
	}
}