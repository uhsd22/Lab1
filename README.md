# <center><b>Метрические алгоритмы</b></center>

**Гипотеза компактности** - в задачах классификации предположение о том, что схожие объекты гораздо чаще лежат в одном классе,
чем в разных; или, другими словами, что классы образуют компактно локализованные подмножества в пространстве объектов.

Основываясь на данной гипотезе, реализуем алгоритм **k-ближайших соседей**.


### **1. Метод классификации "kNN" k-ближайших соседей:**

**KNN** сохраняет размеченные тренировочные данные.
- Когда появляются новые неразмеченные данные, kNN проходит по 2 базовым шагам:
	+ Сначала он ищет k ближайших размеченных точек данных – другими словами, k ближайших соседей.
	+ Затем, используя классы соседей, kNN решает, как лучше классифицировать новые данные.

В реализованном методе выбрана евклидова метрика. 
'''
 Distance <- function(u, v) {
  sqrt(sum((u - v)^2))
}
'''
 
В качестве выборки был взят набор "Ирисы Фишера".  
Оптимальное значение параметра k определяют по критерию скользящего контроля с исключением объектов по одному (leave-one-out, LOO). Для каждого объекта проверяется, правильно ли он классифицируется по своим k ближайшим соседям.   
Оценка скользящего контроля LOO алгоритма k-ближайших соседей для данного набора показала, что классификация более точна при k=6.  
График оценки скользящего контроля, а также карта классификации выглядят следующим образом:
![LOO](https://github.com/uhsd22/Lab1/blob/master/LabIMG/LOO(k).png)
![KNN](https://github.com/uhsd22/Lab1/blob/master/LabIMG/map_KNN.png)

### **2. Алгоритм классификации "kwNN" k-взвешенных соседей:**  
Недостаток kNN в том, что максимум может достигаться сразу на нескольких классах. В задачах с двумя классами этого можно избежать, если взять нечётное k. Более общая тактика, которая годится и для случая многих классов — ввести строго убывающую последовательность вещественных весов ![w_i](https://latex.codecogs.com/gif.latex?w_i)
, задающих вклад i-го соседа в классификацию.  
Все объекты выборки сортируются по удаленности от классифицируемого объекта. Выбираются k ближайших соседей.
Классифицируемый объект относим к тому классу, суммарный вес которого больше.
В реализованном методе выбрана евклидова метрика.  
В качестве выборки был взят набор "Ирисы Фишера".  
В качестве последовательности весов взята нелинейно убывающую последовательность - геометрическая прогрессия: ![w_i = q^i](https://latex.codecogs.com/gif.latex?w_i%20%3D%20q%5Ei), где знаменатель прогрессии ![q ∈ (0, 1)](https://latex.codecogs.com/gif.latex?q%20%5Cin%20%280%2C%201%29%24) является параметром алгоритма. Его можно подбирать по критерию LOO, аналогично числу соседей k.  

Для составления карты классификации параметр k равен 6, а параметр веса q равен 0.5:
![LOOKWNN](https://github.com/uhsd22/Lab1/blob/master/LabIMG/LOO(k%2Cq).png)
![KWNN](https://github.com/uhsd22/Lab1/blob/master/LabIMG/map_KWNN.png)

На следующем графике наглядно продемонстрированно превосходство алгоритма классификации kwNN над алгоритмом kNN:
![KNN_KWNN](https://github.com/uhsd22/Lab1/blob/master/LabIMG/ExKNN.png)
![KWNN_KNN](https://github.com/uhsd22/Lab1/blob/master/LabIMG/ExKWNN.png)
