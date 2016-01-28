//
// TableWidget.java
//
//  Created by Unya (see http://newlispfanclub.alh.net/forum/)
//  Functions for row and column removal by Ferry de Bruin (FdB)
//
//
//    Copyright (C) 2016 Lutz Mueller
//
//    This program is free software: you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with this program.  If not, see <http://www.gnu.org/licenses/>.
//


import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.table.*;
import javax.swing.table.DefaultTableModel;
import java.util.*;
import java.io.UnsupportedEncodingException;

import java.util.Enumeration;

class LineNumberTable extends JTable
{
	public static final long serialVersionUID = 1L;
	private JTable mainTable;
 
	public LineNumberTable(JTable table)
	{
	super();
	mainTable = table;
	setAutoCreateColumnsFromModel( false );
	setModel( mainTable.getModel() );
	setSelectionModel( mainTable.getSelectionModel() );
	setAutoscrolls( false );
 
	addColumn( new TableColumn() );
	getColumnModel().getColumn(0).setCellRenderer( mainTable.getTableHeader().getDefaultRenderer() );
	getColumnModel().getColumn(0).setPreferredWidth(50);
	setPreferredScrollableViewportSize(getPreferredSize());
	}

	public boolean isCellEditable(int row, int column) {
	return false;
	}
 
	public int getRowCount() {
		return mainTable.getRowCount() ;
	}

	public Object getValueAt(int row, int column) {
	return new Integer(row + 1);
	}
 
	public int getRowHeight(int row) {
	return mainTable.getRowHeight();
	}
}

@SuppressWarnings("unchecked")
public class TableWidget extends gsObject {
	JScrollPane scrollpane;
	Vector columnNames ;
	DefaultTableModel tableModel;
	JTable mainTable ;
	JTable table ;
	TableColumnModel columnModel ;
	int lastSelectCellRow = -1 ;
	int lastSelectCellCol = -1 ;

	public TableWidget(StringTokenizer params) {
	id = params.nextToken() ;
	action = params.nextToken() ;

	columnModel = new DefaultTableColumnModel() {
		public static final long serialVersionUID = 1L;
		boolean first = true;

		public int getRowCount() {
			TableModel tm = table.getModel() ;
			return tm.getRowCount() ;
			}

		public void addColumn(TableColumn tableColumn) {
			if (first) {
				tableColumn.setMaxWidth(tableColumn.getPreferredWidth());
				super.addColumn(tableColumn);
				tableColumn.setMaxWidth(40);

				DefaultTableCellRenderer renderer = new DefaultTableCellRenderer();
				renderer.setHorizontalAlignment(JLabel.RIGHT) ;
				tableColumn.setCellRenderer(renderer) ;

				first = false;
				}
			}
		};

	columnNames = makeColumns(params) ;

	tableModel = new DefaultTableModel(columnNames, 0);

	mainTable = new JTable(tableModel);

	table = new LineNumberTable(mainTable) ;

	JScrollPane jsp = new JScrollPane(mainTable);
	jsp.setRowHeaderView(null) ;
	jsp.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED) ;
	//jsp.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);

	component = jsp ;
	container = jsp ;
	jcomponent = jsp ;
	scrollpane = jsp ;

	mainTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
	mainTable.setCellSelectionEnabled(true);

	mainTable.setShowGrid(true);
	mainTable.setGridColor(Color.lightGray);
	table.setShowGrid(true);
	table.setGridColor(Color.lightGray);

	// row selection change
	mainTable.getSelectionModel().addListSelectionListener(new cellSelectListener());
	// column selection change
	mainTable.getColumnModel().getSelectionModel().addListSelectionListener(new cellSelectListener());

	//mainTable.getModel().addTableModelListener(new TableChangeListener()) ;

	gsObject.widgets.put(id, this);
	}

	private class cellSelectListener implements ListSelectionListener {
	  public void valueChanged(ListSelectionEvent event) {
		if (event.getValueIsAdjusting()) {
			// The mouse button has not yet been released
			return;
			}
		
		int row = mainTable.getSelectionModel().getLeadSelectionIndex() ;
		int col = mainTable.getColumnModel().getSelectionModel().getLeadSelectionIndex() ;

		Object data = "===="; // base 64 for empty string

		if(row < 0 || col < 0) return;

		if (lastSelectCellRow == row && lastSelectCellCol == col) { return ; }

		lastSelectCellRow = row ;
		lastSelectCellCol = col ;

		data = getCell(row, col);
		if (action != null) {
			String text = String.format("(%s \"%s\" %d %d (base64-dec [text]%s[/text]))", 
				action, id , row, col, data) ;
			guiserver.out.println(text) ;
			guiserver.out.flush() ;
			}	
		}
	}

/*
	private class TableChangeListener implements TableModelListener {
	public void tableChanged(TableModelEvent e) {
		switch (e.getType()) {
			case TableModelEvent.INSERT :
			case TableModelEvent.UPDATE :
				int tableRows = mainTable.getRowCount() ;
				int tableCols = mainTable.getColumnCount() ;

				if (action != null) {
					String text = String.format("(%s \"%s\" %d %d )", 
						action, id, tableRows, tableCols) ;
					guiserver.out.println(text) ;
					guiserver.out.flush() ;

					// debug
					System.out.println("guiserver tablechanged listener " + text) ;
					}
				break ;
			default :
				break ;
			}
		}
	}
*/

	String getCell(int row, int col) {
		Object value  = null;
		int tableRows = mainTable.getRowCount() ;
		int tableCols = mainTable.getColumnCount() ;
		String text ;

		if (row >= 0 && col >= 0 && row < tableRows && col < tableCols)
			value = mainTable.getValueAt(row, col) ;

		if (value == null) value = "" ;


		if (guiserver.UTF8)
			text = Base64Coder.encodeStringUTF8(value.toString()) ;
		else
			text = Base64Coder.encodeString(value.toString()) ;

		return text ;
	}

	// (gs:table-set-cell ID row col Value)
	public void tableSetCell(StringTokenizer params) {
		int row = Integer.parseInt(params.nextToken()) ;
		int col = Integer.parseInt(params.nextToken()) ;
		String value = Base64Coder.decodeString(params.nextToken()) ;
		String oldvalue = "====";

		int tableRows = mainTable.getRowCount() ;
		int tableCols = mainTable.getColumnCount() ;

		if (row >= 0 && col >= 0 && row < tableRows && col < tableCols)
			{
			oldvalue = getCell(row, col) ;

			DefaultTableModel tm = (DefaultTableModel)(mainTable.getModel()) ;
			tm.setValueAt(value, row, col) ;
			}

		String text = "(set 'gs:table-cell (base64-dec [text]" + oldvalue + "[/text]))" ;
		guiserver.out.println(text) ;
		guiserver.out.flush();
	}

	// (gs:table-get-cell ID row col)
	public void tableGetCell(StringTokenizer params) {
		int row = Integer.parseInt(params.nextToken()) ;
		int col = Integer.parseInt(params.nextToken()) ;
		String value ;
		String text ;

		value = getCell(row, col) ;

		text = "(set 'gs:table-cell (base64-dec [text]" + value + "[/text]))" ;
		guiserver.out.println(text) ;
		guiserver.out.flush();
	}

	// (gs:table-get ID)
	public void tableGetValueAll(StringTokenizer params) {
		int tableRows = mainTable.getRowCount() ;
		int tableCols = mainTable.getColumnCount() ;

		String text = "(set 'gs:table-full '(";
		for(int row = 0 ; row < tableRows ; row++) {
			text += "(" ;
			for(int col = 0 ; col < tableCols ; col++) {
				text += "[text]" + getCell(row, col) + "[/text] " ;
				}
			text += ") " ;
		}
		text += ") )" ;

		guiserver.out.println(text) ;
		guiserver.out.flush();
	}

	// (gs:table-size ID)
	public void tableGetSize(StringTokenizer params) {
		String text = String.format("(set 'gs:table-size '(%d %d))"
				    , mainTable.getRowCount()
				    , mainTable.getColumnCount()) ;

		guiserver.out.println(text) ;
		guiserver.out.flush();
	}

	// (gs:table-add-row ID)
	// (gs:table-add-row ID ColumnValue0 ColumnValue1 ...)
	public void tableAddRow(StringTokenizer params) {
		Vector v = params2vector(params) ;

		DefaultTableModel tm = (DefaultTableModel)(mainTable.getModel()) ;
		tm.addRow(v) ;
	}
	
	//FdB
	// (gs:table-remove-row ID row)
	public void tableRemoveRow(StringTokenizer params) {
		int row = Integer.parseInt(params.nextToken()) ;
		
		DefaultTableModel tm = (DefaultTableModel)(mainTable.getModel()) ;
		tm.removeRow(row) ;
	}
	
	//FdB
	// (gs:table-set-row-count ID row)
	public void tableSetRowCount(StringTokenizer params) {
		int count = Integer.parseInt(params.nextToken()) ;
		
		DefaultTableModel tm = (DefaultTableModel)(mainTable.getModel()) ;
		tm.setRowCount(count) ;
	}
		
	//FdB
	// (gs:table-set-column-name ID)
	// (gs:table-set-column-name ID ColumnName ColumnName ...)
	public void tableSetColumnIdentifiers(StringTokenizer params) {
		Vector v = params2vector(params) ;
		
		DefaultTableModel tm = (DefaultTableModel)(mainTable.getModel()) ;
		tm.setColumnIdentifiers(v) ;
	}

	// (gs:table-add-column ID)
	// (gs:table-add-column ID ColumnName0 ColumnName1 ...)
	public void tableAddColumn(StringTokenizer params) {
		while(params.hasMoreTokens()) {
			String token = params.nextToken() ;
			String name ;

			if (guiserver.UTF8)
				name = Base64Coder.decodeStringUTF8(token) ;
			else
				name = Base64Coder.decodeString(token) ;

// outcommented for 10.5.7 GS v.1.51, allows to do headerless tables
//			if (name.length() == 0)
//				name = String.format("%d", mainTable.getColumnCount() + 1) ;
			DefaultTableModel tm = (DefaultTableModel)(mainTable.getModel()) ;
			tm.addColumn(name);
		}
	}

	// (gs:table-column ID ColumnNumber Width)
	// (gs:table-column ID ColumnNumber Width Justification)
	// Justification : "left"(default) "center" "right"
	public void tableSetColumn(StringTokenizer params) {
		int colNumber = Integer.parseInt(params.nextToken()) ;
		int colWidth = Integer.parseInt(params.nextToken()) ;

		mainTable.getColumnModel().getColumn(colNumber).setPreferredWidth(colWidth) ;

		if (params.hasMoreTokens()) {
			String strJust = params.nextToken() ;

			DefaultTableCellRenderer renderer = new DefaultTableCellRenderer();

			if (strJust.equals("center"))
				renderer.setHorizontalAlignment(JLabel.CENTER) ;
			else if (strJust.equals("right"))
				renderer.setHorizontalAlignment(JLabel.RIGHT) ;
			else
				renderer.setHorizontalAlignment(JLabel.LEFT) ;

			mainTable.getColumnModel().getColumn(colNumber).setCellRenderer(renderer) ;
		}
	}

	public void tableShowRowNumber(StringTokenizer params) {
		boolean bHeader = params.nextToken().equals("true") ;

		if (bHeader)
			scrollpane.setRowHeaderView(table) ;
		else
			scrollpane.setRowHeaderView(null) ;

		/*
		boolean bScrollbar = params.nextToken().equals("true") ;
		if (bScrollbar)
			scrollpane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED) ;
		else
			scrollpane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER) ;
		*/
	}

	Vector makeColumns(StringTokenizer params) {
		Vector v = params2vector(params) ;

		if (v.isEmpty())
			v.add("column 0") ;
		return v ;
	}

	Vector params2vector(StringTokenizer params) {
		Vector v = new Vector() ;

		while(params.hasMoreTokens()) {
			String token = params.nextToken() ;
			String value ;
			if (guiserver.UTF8)
				value = Base64Coder.decodeStringUTF8(token) ;
			else
				value = Base64Coder.decodeString(token) ;

			v.add(value) ;
			}
		return v ;
	}

}

// eof
