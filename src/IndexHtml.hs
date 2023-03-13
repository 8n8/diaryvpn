module IndexHtml (indexHtml) where

import Data.Text (Text)

indexHtml :: Text
indexHtml =
  "<!DOCTYPE html>\n\
  \<html lang=\"en\">\n\
  \  <head>\n\
  \    <meta charset=\"utf-8\" />\n\
  \    <meta name=\"viewport\" content=\"width=device-width\" />\n\
  \    <title>Diary</title>\n\
  \    <style>\n\
  \      html {\n\
  \        font-family: sans-serif;\n\
  \      }\n\
  \      label {\n\
  \        font-size: 1.5rem;\n\
  \      }\n\
  \      form {\n\
  \        display: flex;\n\
  \        flex-direction: column;\n\
  \        row-gap: 0.5rem;\n\
  \      }\n\
  \      textarea {\n\
  \        font-family: sans-serif;\n\
  \        font-size: 1.5rem;\n\
  \      }\n\
  \      textarea:focus {\n\
  \        outline: blue solid 2px;\n\
  \      }\n\
  \      input {\n\
  \        width: fit-content;\n\
  \        font-size: 1.5rem;\n\
  \        font-family: sans-serif;\n\
  \      }\n\
  \      input:focus {\n\
  \        outline: blue solid 2px;\n\
  \      }\n\
  \      a:focus {\n\
  \        outline: blue solid 2px;\n\
  \      }\n\
  \      body {\n\
  \        display: flex;\n\
  \        flex-direction: column;\n\
  \        row-gap: 1rem;\n\
  \      }\n\
  \      ul {\n\
  \        display: flex;\n\
  \        flex-direction: row;\n\
  \        list-style-type: none;\n\
  \        padding: 0;\n\
  \        margin: 0;\n\
  \        column-gap: 1rem;\n\
  \        font-size: 1.5rem;\n\
  \      }\n\
  \    </style>\n\
  \  </head>\n\
  \  <body>\n\
  \    <nav>\n\
  \      <ul>\n\
  \        <li><a href=\"/read\">Read</a></li>\n\
  \        <li><a href=\"/\">Write</a></li>\n\
  \      </ul>\n\
  \    </nav>\n\
  \    <form action=\"/newentry\" method=\"POST\">\n\
  \      <label for=\"newentry\">Type a new diary entry here:</label>\n\
  \      <textarea\n\
  \        id=\"newentry\"\n\
  \        name=\"newentry\"\n\
  \        rows=\"5\"\n\
  \        cols=\"20\"></textarea>\n\
  \      <input type=\"submit\" value=\"Submit\">\n\
  \    </form>\n\
  \  </body>\n\
  \</html>\n\
  \"
