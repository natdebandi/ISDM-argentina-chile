#!/usr/bin/env python3
"""
Detecta etiquetas de texto de los microdatos EPH que el pipeline NO contempla.

Motivo
------
`1_1_procesamiento_EPH.R` normaliza variables categóricas con `case_when()`
comparando contra etiquetas de texto literales. Cuando la redacción de una
etiqueta cambia entre años, ninguna comparación coincide, el valor cae al
`TRUE ~ as.numeric(as.character(x))` final y se convierte en NA. El dato no se
pierde con un error: se pierde en silencio, y el indicador pasa a calcularse
sobre otra población.

Caso que motivó este script (detectado el 2026-09-13)
-----------------------------------------------------
En 2024 el valor de ESTADO es `"Menor de 10 anios."` (con i, sin ñ, con punto).
El script solo contemplaba `"Menor de 10 años"` y `"Menor de 10 anos"`. Los
menores de 10 años quedaron como NA, el filtro que arma el denominador de la
tasa de actividad los descartó, y en 2024 ese indicador pasó a medirse sobre una
población distinta que en el resto de la serie.

Qué hace
--------
Para cada variable de interés, extrae las etiquetas de texto que el script R
compara, las contrasta con los valores realmente presentes en los microdatos
crudos de un año dado, e informa:

  - valores del dato que el script NO contempla (candidatos a convertirse en NA)
  - etiquetas que el script espera y no aparecen en el dato (código muerto)

Uso
---
    python3 diagnosticos/etiquetas_no_contempladas.py [anio] [ruta_script_R] [ruta_datos_EPH]

Salida: reporte a stdout. Código de salida 1 si encontró valores no contemplados.
"""
import sys, os, re, csv, collections

ANIO = sys.argv[1] if len(sys.argv) > 1 else "2024"
SCRIPT = sys.argv[2] if len(sys.argv) > 2 else os.path.join(
    os.path.dirname(os.path.dirname(os.path.abspath(__file__))),
    "1_1_procesamiento_EPH.R")
RAIZ_DATOS = sys.argv[3] if len(sys.argv) > 3 else os.path.expanduser(
    "~/investigacion/datos/EPH")

VARIABLES = ["ESTADO", "CAT_OCUP", "PP07H", "CH08", "NIVEL_ED",
             "CH04", "CH06", "CH15", "CH16"]


def etiquetas_del_script(ruta):
    """Devuelve {variable: set(etiquetas)} a partir de los case_when del script.

    Heurística: dentro de cada bloque `VAR = case_when(...)`, recoge los literales
    comparados con `==` en las condiciones.
    """
    with open(ruta, encoding="utf-8") as f:
        texto = f.read()

    resultado = {v: set() for v in VARIABLES}
    for variable in VARIABLES:
        # Bloque:  VARIABLE = case_when( ... hasta el cierre al mismo nivel
        patron = re.compile(
            rf"\b{re.escape(variable)}\s*=\s*case_when\(", re.MULTILINE)
        for m in patron.finditer(texto):
            inicio = m.end()
            # avanzar contando paréntesis hasta cerrar el case_when
            nivel, i = 1, inicio
            while i < len(texto) and nivel > 0:
                if texto[i] == "(":
                    nivel += 1
                elif texto[i] == ")":
                    nivel -= 1
                i += 1
            bloque = texto[inicio:i]
            # literales en condiciones:  X == "texto"
            for lit in re.findall(r'==\s*"([^"]*)"', bloque):
                resultado[variable].add(lit)
    return resultado


def valores_del_dato(ruta, variables):
    """Cuenta valores por variable en una sola pasada sobre el CSV."""
    conteos = {v: collections.Counter() for v in variables}
    with open(ruta, newline="", encoding="utf-8", errors="replace") as f:
        cabecera = f.readline()
        f.seek(0)
        sep = ";" if cabecera.count(";") > cabecera.count(",") else ","
        lector = csv.DictReader(f, delimiter=sep)
        presentes = [v for v in variables
                     if lector.fieldnames and v in lector.fieldnames]
        for fila in lector:
            for v in presentes:
                conteos[v][(fila.get(v) or "").strip()] += 1
    return conteos, presentes


def es_numerico(s):
    if s == "":
        return True
    try:
        float(s.replace(",", "."))
        return True
    except ValueError:
        return False


def main():
    print(f"Etiquetas no contempladas por el pipeline")
    print(f"  script : {SCRIPT}")
    print(f"  datos  : {RAIZ_DATOS}")
    print(f"  año    : {ANIO}\n")

    esperadas = etiquetas_del_script(SCRIPT)
    ruta = os.path.join(RAIZ_DATOS, f"EPH_indiv_{ANIO}.csv")
    if not os.path.exists(ruta):
        print(f"No existe {ruta}")
        return 2

    observadas, presentes = valores_del_dato(ruta, VARIABLES)
    problemas = 0

    for variable in VARIABLES:
        if variable not in presentes:
            continue
        obs = observadas[variable]
        esp = esperadas.get(variable, set())
        # valores no numéricos que el script no contempla
        no_contempladas = sorted(
            v for v in obs
            if v not in esp and not es_numerico(v))
        # etiquetas que el script espera y el dato no trae (código muerto)
        no_aparecen = sorted(e for e in esp if e not in obs)

        if no_contempladas or no_aparecen:
            print(f"--- {variable} ---")
        if no_contempladas:
            problemas += len(no_contempladas)
            print("  VALORES DEL DATO NO CONTEMPLADOS (caen a NA):")
            for v in no_contempladas:
                print(f"    {obs[v]:>8}  {v!r}")
        if no_aparecen:
            print("  etiquetas esperadas por el script que el dato no trae:")
            for e in no_aparecen:
                print(f"              {e!r}")
        if no_contempladas or no_aparecen:
            print()

    print("=" * 68)
    if problemas:
        print(f"RESULTADO: {problemas} valores del dato quedarían como NA.")
        print("Hay que agregarlos al case_when correspondiente.")
    else:
        print("RESULTADO: todas las etiquetas del dato están contempladas.")
    print("=" * 68)
    return 1 if problemas else 0


if __name__ == "__main__":
    sys.exit(main())
