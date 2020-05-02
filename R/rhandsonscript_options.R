# add Download to .csv option
csv <- list(name = "Download to CSV",
            callback = htmlwidgets::JS(
              "function (key, options) {
                         var csv = csvString(this);
                         var link = document.createElement('a');
                         link.setAttribute('href', 'data:text/plain;charset=utf-8,' +
                           encodeURIComponent(csv));
                         link.setAttribute('download', 'data.csv');
                         document.body.appendChild(link);
                         link.click();
                         document.body.removeChild(link);
                       }"))