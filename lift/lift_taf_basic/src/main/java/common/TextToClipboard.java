package common;

import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.StringSelection;

public class TextToClipboard {
	
	public void setText(String text)
	{
		StringSelection stringSelection = new StringSelection(text);
		Clipboard clip = java.awt.Toolkit.getDefaultToolkit().getSystemClipboard();
		clip.setContents(stringSelection, stringSelection);		
	}

}
