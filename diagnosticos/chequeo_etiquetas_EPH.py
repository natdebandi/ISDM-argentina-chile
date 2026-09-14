#!/usr/bin/env python3
"""
Chequeo de etiquetas de texto en los microdatos crudos de la EPH (INDEC).

Motivo
------
El pipeline de procesamiento (1_1_procesamiento_EPH.R) normaliza variables con
case_when() sobre etiquetas de TEXTO. Si la redacción de una etiqueta cambia
entre años, la comparación no coincide, el valor cae al TRUE final y se
convierte en NA. Eso introduce una ruptura silenciosa en la serie: el indicador
deja de medir lo mismo a lo largo del tiempo sin que nada lo señale.

Caso detectado el 2026-09-13
---------------------------
En 2024 el valor de ESTADO es "Menor de 10 anios." (con i, sin ñ, con punto).
El script solo contempla "Menor de 10 años" y "Menor de 10 anos". Como ninguna
coincide, los menores de 10 años quedan como NA. El filtro que arma el
denominador de la tasa de actividad los descarta, y en 2024 el indicador pasa a
calcularse sobre una población distinta que en el resto de la serie.

Uso
---
    python3 diagnosticos/chequeo_etiquetas_EPH.py [ruta_datos_EPH]

Salida: vocabulario por variable y año, y las etiquetas que aparecen o
desaparecen entre años consecutivos.
"""
import sys, os, csv, collections

RAIZ = sys.argv[1] if len(sys.argv) > 1 else os.path.expanduser(
    "~/investigacion/datos/EPH")
VARIABLES = ["ESTADO", "CAT_OCUP", "PP07H", "CH08", "NIVEL_ED"]
ANIOS = range(2016, 2025)


def vocabulario(ruta, variables):
    """Devuelve {variable: Counter(etiqueta -> n)} en una sola pasada."""
    conteos = {v: collections.Counter() for v in variables}
    with open(ruta, newline="", encoding="utf-8", errors="replace") as f:
        cabecera = f.readline()
        f.seek(0)
        sep = ";" if cabecera.count(";") > cabecera.count(",") else ","
        lector = csv.DictReader(f, delimiter=sep)
        presentes = [v for v in variables if lector.fieldnames and v in lector.fieldnames]
        for fila in lector:
            for v in presentes:
                conteos[v][(fila.get(v) or "").strip()] += 1
    return conteos


def main():
    print("Chequeo de etiquetas de texto en microdatos EPH")
    print(f"Datos: {RAIZ}\n")

    por_anio = {}
    for anio in ANIOS:
        ruta = os.path.join(RAIZ, f"EPH_indiv_{anio}.csv")
        if not os.path.exists(ruta):
            print(f"  {anio}: archivo ausente")
            continue
        print(f"  leyendo {anio}...", flush=True)
        por_anio[anio] = vocabulario(ruta, VARIABLES)

    if not por_anio:
        print("\nNo se leyó ningún año. Revisá la ruta.")
        return

    anios = sorted(por_anio)

    print("\n" + "=" * 72)
    print("VOCABULARIO POR VARIABLE Y AÑO")
    print("=" * 72)
    for variable in VARIABLES:
        print(f"\n--- {variable} ---")
        for anio in anios:
            c = por_anio[anio].get(variable)
            if not c:
                print(f"  {anio}: columna ausente")
                continue
            etiquetas = ", ".join(f"{k!r}:{v}" for k, v in c.most_common(10))
            print(f"  {anio}: {etiquetas}")

    print("\n" + "=" * 72)
    print("CAMBIOS DE VOCABULARIO ENTRE AÑOS CONSECUTIVOS")
    print("=" * 72)
    print("(una etiqueta nueva en el año b puede ser una redacción distinta de la")
    print(" misma categoría, o una categoría genuinamente nueva: hay que mirarlo)\n")
    hubo = False
    for variable in VARIABLES:
        for a, b in zip(anios, anios[1:]):
            ca = por_anio[a].get(variable)
            cb = por_anio[b].get(variable)
            if not ca or not cb:
                continue
            nuevas = set(cb) - set(ca)
            idas = set(ca) - set(cb)
            if nuevas or idas:
                hubo = True
                print(f"{variable}  {a} -> {b}")
                if nuevas:
                    print(f"    aparecen:    {sorted(nuevas)}")
                if idas:
                    print(f"    desaparecen: {sorted(idas)}")
    if not hubo:
        print("Sin cambios de vocabulario entre años consecutivos.")

    print("\n" + "=" * 72)
    print("CHEQUEO CRUZADO CONTRA LAS ETIQUETAS QUE ESPERA EL PIPELINE")
    print("=" * 72)
    esperadas = {
        "ESTADO": ["Entrevista individual no realizada (no respuesta al cuestionario individual)",
                   "Ocupado", "Desocupado", "Inactivo",
                   "Menor de 10 años", "Menor de 10 anos"],
        "PP07H": ["Si", "Sí", "No", "Ns./Nr.", "Ns./Nr..", "NS./NR."],
        "CAT_OCUP": ["Patrón", "Patron", "Obrero o empleado", "Cuenta propia",
                     "Trabajador familiar sin remuneracion",
                     "Trabajador familiar sin remuneración", "Ns/Nr", "Ns./Nr.", "Ns./Nr.."],
    }
    for variable, lista in esperadas.items():
        print(f"\n--- {variable} ---")
        for anio in anios:
            c = por_anio[anio].get(variable)
            if not c:
                continue
            observadas = set(c)
            faltantes = [e for e in lista if e not in observadas]
            # etiquetas observadas que no están contempladas y no son numéricas
            no_contempladas = sorted(
                e for e in observadas
                if e not in lista and e != "" and not e.replace(".", "", 1).isdigit())
            if no_contempladas:
                print(f"  {anio}: NO contempladas por el script -> {no_contempladas}")

    print("\nFin del chequeo.")


if __name__ == "__main__":
    main()
