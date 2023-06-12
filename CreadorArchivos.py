import os

# Directorio donde se crearán los archivos
directorio = "./archivos"

# Verificar si el directorio existe, si no, crearlo
if not os.path.exists(directorio):
    os.makedirs(directorio)

# Contenido del archivo
contenido = """let a = 1+2
//comentario
for(let i = 0; i < 5; i = i+1)
{
    let b = 2
    b = 1+i
}"""

string_lista = "["
# Generar 100 archivos .txt
for i in range(1, 101):
    nombre_archivo = f"archivo{i}.txt"
    string_lista += '"archivos/'+nombre_archivo+'" '
    ruta_archivo = os.path.join(directorio, nombre_archivo)
    
    with open(ruta_archivo, "w") as archivo:
        archivo.write(contenido)
string_lista += "]"
print(string_lista)
