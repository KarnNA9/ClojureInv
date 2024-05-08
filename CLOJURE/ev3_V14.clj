(ns CLOJURE.ev3-V14)
(require '[clojure.java.io :as io])
;__________________________________________________________________________________________________________________________________________________
; 1 - Lectura, declaración y manejo de archivos ---------------------------------------------------------------------------------------------------

(def inventarios ; lista con las direcciones de los inventarios
  (list
   "CLOJURE/invent/1.txt"
   "CLOJURE/invent/2.txt"
   "CLOJURE/invent/3.txt"
   "CLOJURE/invent/4.txt"
   "CLOJURE/invent/5.txt"
   "CLOJURE/invent/6.txt"
   "CLOJURE/invent/7.txt"
   "CLOJURE/invent/8.txt" 
   "CLOJURE/invent/9.txt"
   "CLOJURE/invent/10.txt"))

(def posiciones ; lista con las direcciones de las posiciones de cada carrusel
  (list
   "CLOJURE/pos/1.txt"
   "CLOJURE/pos/2.txt"
   "CLOJURE/pos/3.txt"
   "CLOJURE/pos/4.txt"
   "CLOJURE/pos/5.txt"
   "CLOJURE/pos/6.txt"
   "CLOJURE/pos/7.txt"
   "CLOJURE/pos/8.txt"
   "CLOJURE/pos/9.txt"
   "CLOJURE/pos/10.txt"))

;__________________________________________________________________________________________________________________________________________________
; 1.1 - Set y Get ---------------------------------------------------------------------------------------------------------------------------------

; n=iteración, xy=tope de iteraciones (depende del valor de fila/columna o ID del elemento buscado). SIEMPRE DEBE LLAMARSE CON n=1
(defn avanza [n xy lista] ; funcion auxiliar para búsquedas que impliquen recorrer una lista 
  (if (= n xy) (first lista) (avanza (+ n 1) xy (rest lista))))

(defn getInv [id]     ; dado el ID, devuelve la dirección del carrusel con es ID buscado
  (avanza 1 id inventarios))

(defn posicion [id]   ; dado el ID, devuelve la dirección del carrusel con es ID buscado
  (avanza 1 id posiciones))

(defn inventario [id] ; lee el carrusel id (int) y lo devuelve como lista
  (with-open [archivo (io/reader (getInv id))] ; EJ.: (inventario 1) regresa el contenido del inventario 1 como lista
    (read-string (apply str (line-seq archivo)))))

(defn getPos [id]     ; lee el carrusel id (int) y lo devuelve como lista
  (with-open [archivo (io/reader (posicion id))] ; EJ.: (inventario 1) regresa el contenido del inventario 1 como lista
    (read-string (apply str (line-seq archivo)))))

(defn setPos [p id]   ; cambia la posición p del carrusel dir
  (with-open [out (io/writer (posicion id))]
    (spit out p)))

(map #(setPos 101 %) (range 1 (+ 1 (count posiciones)))) ; Asigna la posición 101 a todos los carruseles por defecto

(defn getProd [id]          ; Regresa el producto de (inventario id) correspondiente al estado actual de su autómata
  (avanza 1 (mod (getPos id) 100)                                             ; (mod (getPos) 100)             -> col
          (avanza 1 (int (Math/floor (/ (getPos id) 100))) (inventario id)))) ; (Math/floor (/ (getPos) 100))  -> fil

(defn lugar [id] ; Imprime en qué carrusel se está haciendo la transacción
  (newline) (print "____________________________________") (newline)
  (print "CARRUSEL/INVENTARIO" id "--------------") (newline))

;___________________________________________________________________________________________________________________________________________________
; 2 - Creación y manejo de Identificadores ---------------------------------------------------------------------------------------------------------

(defn col [id] ; Da un entero con la cantidad total de filas del carrusel id
  (count (first (inventario id)))) ; Ej.: (col 1) regresa la cantidad de filas del inventario id

(defn fil [id] ; Da un entero con la cantidad total de columnas del carrusel id
  (count (inventario id))) ; Ej.: (fil 1) regresa la cantidad de columnas del inventario 1

;___________________________________________________________________________________________________________________________________________________
; 3 - Funciones de movimiento. Lee y ejecuta el autómata -------------------------------------------------------------------------------------------
; Mover-carrusel Recibe la entrada y la posición actual en el carrusel -----------------------------------------------------------------------------
; A^  Bv  I<  D> -----------------------------------------------------------------------------------------------------------------------------------

(defn Arr [id]                                ; Sube una fila 
  (if (and (<= (getPos id) (+ 100 (col id))) ; Si pertenece a la primer fila, 
           (>= (getPos id) 101))             ; sube a la última
    (setPos (+ (getPos id) (* (- (fil id) 1) 100)) id)
    (setPos (- (getPos id) 100) id)))

(defn Baj [id]                                            ; Baja una fila 
  (if (and (<= (getPos id) (+ (* (fil id) 100) (col id))) ; Si pertenece a la última fila, 
           (>= (getPos id) (* (fil id) 100)))             ; baja a la primera 
    (setPos (- (getPos id) (* (- (fil id) 1) 100)) id)
    (setPos (+ (getPos id) 100) id)))

(defn Izq [id]                                                ; Mueve un lugar a la izquierda
  (if (< (Integer/parseInt (str (last (str (getPos id))))) 2) ; Verifica si está en rango para moverse
    (print "ERROR: No se puede mover más a Izq\n")
    (setPos (- (getPos id) 1) id)))

(defn Der [id]                                                       ; Mueve un lugar a la derecha
  (if (= (Integer/parseInt (str (last (str (getPos id))))) (col id)) ; Verifica si está en rango para moverse
    (print "ERROR: No se puede mover más a Der\n")
    (setPos (+ (getPos id) 1) id)))

; Función principal de movimiento. Lee y ejecuta el autómata en el inventario id
(defn Mover-carrusel [ent id] ; ent es una lista ej. '(A A A A D). id es el ID de su carrusel/inventario 
  (cond (empty? ent)
        (do (print "En ventanilla-> ")
            (print (getProd id)))
        (= (first ent) 'A)
        (do (Arr id)
            (print "A^  ") (print (getProd id)) (newline)
            (Mover-carrusel (rest ent) id))
        (= (first ent) 'B)
        (do (Baj id)
            (print "Bv  ") (print (getProd id)) (newline)
            (Mover-carrusel (rest ent) id))
        (= (first ent) 'I)
        (do (Izq id)
            (print "I<  ") (print (getProd id)) (newline)
            (Mover-carrusel (rest ent) id))
        (= (first ent) 'D)
        (do (Der id)
            (print "D>  ") (print (getProd id)) (newline)
            (Mover-carrusel (rest ent) id))
        :else (do (print "ERROR: Entrada no aceptada") nil)))

;__________________________________________________________________________________________________________________________________________________
; 4 - Funciones de búsqueda por nombre ------------------------------------------------------------------------------------------------------------
; dado el nombre y lista (se debe llamar inicialmente con (inventario)), regresa el elemento. Ej, (buscar 'a03 (inventario 1)) -> (a03 64 10)
; Si el nombre dado no existe, regresa una lista vacía --------------------------------------------------------------------------------------------

(defn buscar2 [nombre subl] ; Función auxiliar de (buscar)
  (cond (empty? subl) '()
        (= nombre (first (first subl))) (first subl)
        :else (buscar2 nombre (rest subl))))

(defn buscar [nombre lista]
  (cond (empty? lista) (do (print "El producto no existe") '())
        (empty? (buscar2 nombre (first lista)))
        (buscar nombre (rest lista))
        :else (buscar2 nombre (first lista))))

(defn auxGetID [prod lista n] ; Función auxiliar de getID
  (if (= prod (first lista)) n ; Regresa índiec neto del producto
      (auxGetID prod (rest lista) (+ n 1))))

(defn getID [prod ID] ; Regresa la ID del producto en el id. [prod = (nomProd cant precio) ]
  (let [a (auxGetID prod (apply concat (inventario ID)) 1)
        b (int (Math/floor (/ a (col ID))))]
    (+ (if (= b (/ a (col ID))) (* b 100) (* (+ b 1) 100))
       (if (= (- a (* b (col ID))) 0) (col ID) (- a (* b (col ID)))))))

;__________________________________________________________________________________________________________________________________________________
; 5 - camino/racorrer automáticamente el carrusel -------------------------------------------------------------------------------------------------
; Busca el camino más corto entre el producto en ventanilla y el que se desea retirar/agregar -----------------------------------------------------

(defn camino [pos2 id] ; recibe el ID del segundo producto y el id de su inventario
  (lugar id)
  (defn fila [a] (int (Math/floor (/ a 100))))  ; Fila del producto dada su ID
  (defn colm [b] (mod b 100))                   ; Columna del product
  (def minFila (if (<= (getPos id) pos2)
                 (list (getPos id) pos2)      ; Cuál de los dos productos tiene menor ID = cuál está más "arriba" 
                 (list pos2 (getPos id))))    ; (car minFila) es el de menor ID y (cdr minFila) el mayor

  ; Genera la lista con los movimientos del carrusel (Ej. (genMov 3 'A) -> '(A A A))
  (defn genMov [n m]
    (if (= n 0) '()
        (concat (list m) (genMov (- n 1) m)))) ; n=cantidad, m= letra de mov. (A B I D)
  (Mover-carrusel
   (concat (if (<= (abs (- (fila (last minFila)) (fila (first minFila))))         ; Qué es más corto, bajar
                   (abs (- (+ (fila (first minFila)) (fil id)) (fila (last minFila))))) ; o subir 
             (genMov (abs (- (fila (last minFila)) (fila (first minFila))))
                     (if (= (getPos id) (first minFila)) 'B 'A))
             (genMov (abs (- (+ (fila (first minFila)) (fil id)) (fila (last minFila))))
                     (if (= (getPos id) (first minFila)) 'A 'B))) ; Parte para subir o bajar
           (genMov (abs (- (colm (getPos id)) (colm pos2))) (if (<= (colm (getPos id)) (colm pos2)) 'D 'I))) id)) ; Parte para Der o Izq


;__________________________________________________________________________________________________________________________________________________
; 6 - Retirar y agregar productos -----------------------------------------------------------------------------------------------------------------
; Los movimientos necesarios para llegar al producto donde se desea realizar la transacción se efectúan automáticamente ---------------------------

; nom->nombre, cant->cantidad, s(1->sacar, 2->meter), id->id de inventario
(defn error1 [] (print "ADVERTENCIA: Se retiró la cantidad máxima"))
(defn error2 [] (print "ADVERTENCIA: Se agregó la cantidad máxima"))
(defn retirar? [y cant] (if (> (second y) cant)         (- (second y) cant) 0))
(defn agregar? [y cant] (if (<= (+ (second y) cant) 64) (+ (second y) cant) 64))
(defn actualInvent [nom cant s id] ; Genera el inventario con los cambios
  (map (fn [x]
         (map (fn [y]
                (if (= nom (first y))
                  (list (first y) (if (= s 1) (retirar? y cant) (agregar? y cant)) (last y))
                  y))
              x))
       (inventario id)))
(defn editarProd [nom cant s id] ; < Reescribe el inventario con la información nueva
  (spit (getInv id) (prn-str (actualInvent nom cant s id))))

(defn retirar [& args]
  (cond
    (= 2 (count args))
    (do (lugar (last args))
        (editarProd (first (getProd (last args))) (first args) 1 (last args))
        (println "\nRetirado    -> " (getProd (last args)))
        (if (= (second (getProd (last args))) 0)
          (error1) ""))
    :else
    (do (camino (getID (buscar (first args) (inventario (last args))) (last args)) (last args))
        (editarProd (first args) (second args) 1 (last args))
        (println "\nRetirado     -> " (getProd (last args)))
        (if (= (second (getProd (last args))) 0)
          (error1) ""))))

(defn agregar [& args]
  (cond
    (= 2 (count args))
    (do (lugar (last args))
        (editarProd (first (getProd (last args))) (first args) 2 (last args))
        (println "\nAgregado     -> " (getProd (last args)))
        (if (= (second (getProd (last args))) 64)
          (error2) ""))
    :else
    (do (camino (getID (buscar (first args) (inventario (last args))) (last args)) (last args))
        (editarProd (first args) (second args) 2 (last args))
        (println "\nAgregado     -> " (getProd (last args)))
        (if (= (second (getProd (last args))) 64)
          (error2) ""))))

;__________________________________________________________________________________________________________________________________________________
; 7 - Valor total de inventario / poco-inventario  / Top 10 ---------------------------------------------------------------------------------------
; Funciones principales. 

; 7.1.1 Devuelve el valor total del inventario id
(defn valor-total-inventario [id]
  (apply + (map #(* (second %) (last %)) (apply concat (inventario id)))))

; 7.1 Imprime una lista con los vvalores totales de cada inventario/carrusel y devuelve la sumatoria de estos valores
(defn valor-total-almacen []
  (defn a [] (pmap #(valor-total-inventario %) (range 1 (+ 1 (count inventarios))))) ; Se evalúan todos los valores totales de cada inventario en paralelo
  (print "Valor total de carruseles:")
  (print (map (fn [x y] [(newline) (str "  Carrusel " x ": $ " y)]) (range 1 (+ 1 (count inventarios))) (a)))
  (newline) (print "\nValor total del Almacen: $" (apply + (a))) (newline))

; 7.2.1.1 Evalúa qué productos tienen poco inventario (<12). Genera una lista con los nombres de los productos para mostrar los detalles
(defn poco-invent-format [id] ; Para un inventario a la vez
  (map #(if (< (second %) 12)
          (buscar (first %) (inventario id))
          '())
       (apply concat (inventario id))))

; 7.2.1.2 Formato para imprimir los datos de un producto
(defn formato [a id] ; a -> producto (nom cant<12 precio)
  (let [n (getID a id)]
    (newline)
    (print "nombre:-----" (first a)) (newline)
    (print "  cantidad: " (second a)) (newline)
    (print "  ID:" id "-" n) (newline)
    (print "  fila:" (int (Math/floor (/ n 100))))
    (print ", colm:" (mod n 100))
    (print ", carrusel:" id) (newline)
    0))

; 7.2.1 Imprime la lista de los productos con poco inventario en inventario id
(defn poco-inventario-id [id] ; Imprime los datos de los productos con poca cantidad (<12) en el inventario id
  (lugar id)
  (pr (doall (map #(if (not (empty? %)) (formato % id) 0)
                  (poco-invent-format id)))))

; 7.2 Imprime la lista de todos los productos del almacén con poco inventario o 0 si no hay problema con el producto
(defn poco-inventario [] ;Imprime los datos de los productos con poca cantidad (<12) en todos los inventarios
  (print "ADVERTENCIA productos con poca o nula existencia") (newline)
  (pr (doall (map #(poco-inventario-id %) (range 1 (+ 1 (count inventarios)))))))

; 7.3.1 Imprime los n valores más altos de la lista
(defn top [lista n] ;lista-> lista de los valores totales y si ID de inventario
  (if (= 1 n)
    (print n "- Carrusel" (last (last lista)) ": $" (first (last lista)))
    (do (print n "- Carrusel" (last (last lista)) ": $" (last (last lista)))
        (top (rest lista) (- n 1)))))

; 7.3 Top 10% de inventarios con mayor valor total
(defn top10 []
  (print "\n\nTop 10% de inventarios con mayor valor total") (newline)
  (top (sort-by first (map list (map #(valor-total-inventario %) (range 1 (+ 1 (count inventarios))))
                           (range 1 (+ 1 (count inventarios)))))
       (int (Math/floor (/ (count inventarios) 10)))))

;__________________________________________________________________________________________________________________________________________________
; 8 - Ejecución -----------------------------------------------------------------------------------------------------------------------------------
; Lee el archivo de entrada 'entrada.txt', lo ejecuta y al final muestra el valor total del almacén [7.1], Productos con poco inventario [7.2] y el top 10 de carruseles con más valor [7.3]

(defn main []
  ; Regresa el contenido de entrada.txt como lista de strings
  (def a (clojure.string/split-lines (slurp "CLOJURE/entrada.txt")))

  ; Separa la entrada según el inventario, así cada inventario tiene su popia lista sin importar cuantas transacciones se hagan en cada uno
  (defn separarPorInventario [lst]
    (->> lst
         (group-by last) ; El último argumento de cada función es el id de su inventario
         (vals)))

  ; (map #(read-string %) a) devuelve la lista de strings a como lista normal
  (let [comm (separarPorInventario (map #(read-string %) a))]
    ; Se genera un hilo para cada carrusel, el cual sigue sus propias transacciones 
    (pmap (fn [a] (map (fn [b] (eval b)) a)) comm)))

(defn ejecutar []
  (pr (doall (main)))
  (println "\n\nTRANSACCIONES COMPLETADAS. Generando totales...")
  (Thread/sleep 3500)
  (poco-inventario)
  (valor-total-almacen)
  (top10))