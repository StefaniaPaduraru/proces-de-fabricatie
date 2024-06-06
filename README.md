# Sistem de Control Automatizat pentru Procesul de Fabricație

Acest proiect simulează comanda unui proces de fabricație utilizând limbajul CLIPS. Sistemul include diverse componente hardware și software pentru a asigura un flux eficient și automatizat al procesului de fabricație.

## Componente

- **IRB 1400 și IRB 2400**: Roboți utilizați pentru manipularea pieselor.
- **Conveior cu palete**: Sistem de transport al pieselor.
- **Mașină unealtă (PC Mill 55)**: Echipament pentru prelucrarea pieselor.
- **Sistem de vedere artificială (Optimaster)**: Identificarea și controlul calității pieselor.
- **Stocator de piese brute/semifabricate (S1)**: Depozit pentru piesele de prelucrat.
- **Stocator de piese finite (S2)**: Depozit pentru paleții finalizați.
- **Calculatoare**: Pentru controlul și monitorizarea întregului proces.

## Scop

Realizarea a patru paleți finalizați în stocatorul S2, completând cele 14 poziții disponibile pe fiecare palet cu piese de diferite tipuri (B, C, D, E și F). Utilizatorul poate specifica tipurile de piese și configurațiile dorite pentru fiecare palet.

## Funcționalitate

1. **Preluarea pieselor**: Robotul IRB 1400 preia piesele din stocatorul S1 și le plasează în mașina unealtă pentru prelucrare, conform necesităților.
2. **Prelucrarea pieselor**: Mașina unealtă PC Mill 55 transformă piesele conform unuia dintre cele 9 programe disponibile, de la tipul A la F.
3. **Transferul pieselor prelucrate**: Piesele prelucrate sunt transferate pe conveior și apoi pe masa de lucru de către robotul IRB 2400.
4. **Controlul calității**: Sistemul de vedere artificială inspectează piesele pentru a verifica conformitatea sau identifica rebuturile.
5. **Completarea paleților**: Robotul IRB 2400 plasează piesele corecte pe paleții din stocatorul S2. Paleții finalizați sunt transferați în zona de paleți finalizați.

## Limbaj de Programare
Acest proiect este implementat în limbajul CLIPS.
