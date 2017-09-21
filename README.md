# Rdatadownloader
Diese kleine App ermölicht es über die Datastream API Daten herunterzuladen. 

## Benötigte Pakete
Die folgenden Pakete müssen für die App installiert sein:

    install.packages("shiny")
    install.packages("XML")
    install.packages("RCurl")
    install.packages("devtools")
    devtools::install_github("fcocquemas/RDataStream")
    
Die Anweisungen können per copy/paste übernommen werden.

## Anmerkungen
Die folgenden Punkte sind zu beachten:

* Die App hat bisher kein Error handling. Es ist auch keins geplant. Grund: Kostet mich Zeit. Wer Lust hat es zu ergänzen gerne!
* Die App hat bisher nur auf meine Bedürfnisse zugeschnittene Funktionalität. Wer sie erweitern will, kann dies herzlich gerne.
* Alles ist für R geschrieben. Ein paar Zeilen Code machen die ausgegeben Objekte für Matlab kompatibel. Das bleibt aber eine leichte und perfekte Übung zum Zusammenarbeiten über GIT. 

## Bedienung
* Im Upload muss eine .csv Datei hochgeladen werden, die eine Spalte hat mit IDs. Die Spalte *muss* "Symbol" heissen. Als ID kann ein Datastream Symbol oder eine ISIN (ich glaube auch ein paar andere IDs) verwendet werden. Die Spalten der .csv sollen durch ";" getrennt sein. Alle Spalten bis auf Symbol werden aber ignoriert.