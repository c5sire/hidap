output$ui_Pivot <- renderUI({
  help_modal('Pivot','pivotHelp',inclMD(file.path("..",app_dir,"tools/help/pivot.md")))
})

output$pivotData <- rpivotTable::renderRpivotTable({
  rpivotTable::rpivotTable(data = .getdata())
})

