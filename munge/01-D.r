library(RDCOMClient)

# Open a specific workbook in Excel:
xlApp <- COMCreate("Excel.Application")
xlWbk <- xlApp$Workbooks()$Open("S:\\ScholarshipsProject\\R\\ScholarshipProject\\output\\scholarCompare.xlsm")

# this line of code might be necessary if you want to see your spreadsheet:
xlApp[['Visible']] <- FALSE 

# Run the macro called "MyMacro":
xlApp$Run("scholarDiff")

# Close the workbook and quit the app:
xlWbk$Close(FALSE)
xlApp$Quit()

# Release resources:
rm(xlWbk, xlApp)
gc()