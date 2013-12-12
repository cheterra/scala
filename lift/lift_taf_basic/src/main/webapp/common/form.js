// unused
function pageForm2(portal, tableId, sortedBy, functId, topRow, rows, rowId, rowNum, colId)
{
   var e = window.event;

   if (e.keyCode == 13) 
   {
      // ENTER
      pageForm(portal, tableId, sortedBy, functId, topRow, rows, rowId, rowNum, colId);  
   }
}

// unused
function pageForm(portal, tableId, sortedBy, functId, topRow, rows, rowId, rowNum, colId)
{
   var seekVal = "";
   var canSeek = document.TableBrowser.canSeek.value;
   if (document.TableBrowser.inputSeek)
   {
     seekVal = document.TableBrowser.inputSeek.value;
   }
   
   var pageLen = document.TableBrowser.pageLen.value;
   var canSetLen = document.TableBrowser.canSetLen.value;
   if (document.TableBrowser.comboSize)
   {
     var sizeSel = document.TableBrowser.comboSize.selectedIndex;
     if(sizeSel > -1)
     {
        pageLen  = document.TableBrowser.comboSize[sizeSel].text;
     }
   }

   var selFirstRow = document.TableBrowser.selectFirstRow.value;
   var eventId = tableId;
   if(functId == tableId + "_ITEM")
   {
      eventId  = tableId + "_ITEM";
   }

   var actPara = portal  + "&tableId="     + tableId 
                         + "&eventId="     + eventId  
                         + "&FUNCTION="    + functId 
                         + "&SORTEDBY="    + sortedBy 
                         + "&PAGESEEK="    + seekVal 
                         + "&CANSEEK="     + canSeek 
                         + "&PAGELEN="     + pageLen
                         + "&CANSETLEN="   + canSetLen
                         + "&SELFIRSTROW=" + selFirstRow
                         + "&TOPROW="      + topRow  
                         + "&ROWS="        + rows     
                         + "&COLID="       + colId    
                         + "&ROWID="       + rowId    
                         + "&ROWNUM="      + rowNum;

   document.TableBrowser.action = "enterWorkflow.do?" + actPara;
   document.TableBrowser.submit();
}


// unused
function tableForm(tableId)
{
   //var td = document.tableId.tableData.value;
   document.getElementById(tableId).submit();
}


// unused
function toggleDiv()
{
	alert('wir toggeln...');
	//debugger;
	var returnCheckBox = document.getElementById('theCheckbox');
	var returnBox = document.getElementById('theDiv');
	if (returnCheckBox.checked) {
		returnBox.style.display = "block";
	}
	else {
		returnBox.style.display = "none";
	}
}
