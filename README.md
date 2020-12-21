# <center><b>Метрические алгоритмы</b></center>
<center>
<table>
  <tbody>
    <tr>
      <th>Метод</th>
      <th>Параметры</th>
      <th>Точность</th>
    </tr>
    <tr>
      <td><a href="#KWNN">KWNN</a></td>
      <td>k=20, q=0.05</td>
      <td>0.02</td>
    </tr>
    <tr>
      <td><a href="#KNN">KNN</a></td>
      <td>k=6</td>
      <td>0.0333</td>
    </tr>
    <tr>
      <td><a href="#ParzenR">Parzen, Rectangle kerel</a></td>
      <td>h=0.35</td>
      <td>0.04</td>
    </tr>
    <tr>
      <td><a href="#ParzenT">Parzen, Triangle kerel</a></td>
      <td>h=0.35</td>
      <td>0.04</td>
    </tr>
    <tr>
      <td><a href="#ParzenQ">Parzen, Quartic kerel</a></td>
      <td>h=0.35</td>
      <td>0.04</td>
    </tr>
    <tr>
      <td><a href="#ParzenE">Parzen, Epanechnikov kerel</a></td>
      <td>h=0.35</td>
      <td>0.04</td>
    </tr>
    <tr>
      <td><a href="#ParzenG">Parzen, Gauss kerel</a></td>
      <td>h=0.1</td>
      <td>0.04</td>
    </tr>
  </tbody>
   </table>
</center>
Гипотеза компактности - в задачах классификации предположение о том, что схожие объекты гораздо чаще лежат в одном классе,
чем в разных; или, другими словами, что классы образуют компактно локализованные подмножества в пространстве объектов.

Основываясь на данной гипотезе, реализуем алгоритм **k-ближайших соседей** на "Ирисах Фишера".

В реализованных методах выбрана евклидова метрика. 


```
 Distance <- function(u, v) {
  sqrt(sum((u - v)^2))
}
```
## Алгоритм классификации "1NN"(добавлен в качестве дополнения):
![ONN](https://github.com/uhsd22/ML_LABS/blob/master/SBC/1NN/ONN.png)
### **1. Метод классификации k-ближайших соседей("kNN"):**

**KNN** сохраняет размеченные тренировочные данные.
- Когда появляются новые неразмеченные данные, kNN проходит по 2 базовым шагам:
	+ Сначала он ищет k ближайших размеченных точек данных – другими словами, k ближайших соседей.
	+ Затем, используя классы соседей, kNN решает, как лучше классифицировать новые данные.
```
KNN <- function(xx,z,k = 6){
  rows0 <- dim(xx)[1] #ряды
  col0  <- dim(xx)[2] - 1 #стобцы
  distances <- matrix(NA, rows0, col0) #матрица расстояний
  for(i in rows0:1){
    distances[i,] <- c(i, EucDist(xx[i,1:col0], z)) #заполнение матрицы
  }
  classes <- xx[order(distances[,2])[1:k], col0 + 1] #сортировка элементов по частоте появления цветов
  class <- names(which.max(table(classes))) #цвет наиболее близкого класса
  return(class)
}
```
Оптимальное значение параметра k определяют по критерию скользящего контроля с исключением объектов по одному (leave-one-out, LOO). Для каждого объекта проверяется, правильно ли он классифицируется по своим k ближайшим соседям.  
Реализация LOO для KNN:
```
LOOKNN <- function(xx){
  row1 <- dim(xx)[1]
  col1 <- dim(xx)[2] - 1
  score_arr <- rep(0, row1)
  for(i in 1:row1){
    tmp_xx <- xx[-i,]
    tmp_xx_cnt <- dim(tmp_xx)[1]
    dist_matrix <- matrix(NA, tmp_xx_cnt, col1) #задаем матрицу расстояний
    for(j in tmp_xx_cnt:1){
      dist_matrix[j,] <- c(j, EucDist(tmp_xx[j,1:col1], xx[i, 1:col1]))#заполняем матрциу расстояний
    }
    ordered_dist_matrix <- order(dist_matrix[,2]) #сортируем матрицу расстояний
    for (k in 1:row1){  #даем оценку каждому k
      class <- names(which.max(table(xx[ordered_dist_matrix[1:k],col1 + 1])))
      if (class != xx[i, col1 + 1]){
        score_arr[ k ] <- score_arr[ k ] + 1/row1
      }
    }
  }
  ```
![LOO](https://github.com/uhsd22/Lab1/blob/master/LabIMG/LOOKNN.png)
По представленному графику можем заметить, что минимальная оценка **LOO = 0.0(3)** при **k = 6**.
Карта классификации выглядит следующим образом:
![KNN](https://github.com/uhsd22/Lab1/blob/master/LabIMG/map_KNNew.png)

### **2. Метод классификации k-взвешенных соседей("kwNN"):**  
Альтернативный вариант метода kNN: в каждом классе выбирается k ближайших объектов, и объект относится к тому классу, для
которого среднее расстояние до k ближайших соседей минимально.

В функции **LOOKWNN** подберем оптимальный параметр веса q при k = 6.
![LOOKWNN](https://github.com/uhsd22/Lab1/blob/master/LabIMG/KWNNLoo.png)

Для составления карты классификации параметр k равен 6, а параметр веса q равен 0.5:
![KWNN](https://github.com/uhsd22/Lab1/blob/master/LabIMG/map_KWNNew.png)

Далее на графиках наглядно продемонстрированно превосходство алгоритма классификации KWNN над алгоритмом KNN:
### KNN
![KNN_KWNN](https://github.com/uhsd22/Lab1/blob/master/LabIMG/ExKNN.png)
### KWNN
![KWNN_KNN](https://github.com/uhsd22/Lab1/blob/master/LabIMG/ExKWNN.png)
В примере **k = 6**. Так как KWNN в отличии от KNN оценивает не только индекс соседа, но и его расстояние, то результат получается более точный.

По представленному графику можем заметить, что минимальная оценка **LOO = 0.02** при **k = 20**.
![KWNN_KN](https://github.com/uhsd22/Lab1/blob/master/LabIMG/LOOKwNN.png)
### **3.Сравнение качества KNN и KWNN:**  

**KNN** довольно медленный алгоритм, который хранит все данные обучения, по этой причине часто не является эффективным и используется в основном для ознакомления.
**KWNN** отличается от **KNN**, тем что учитывает порядок соседей объекта, что позволяет улучшить качество классификации.


**4. Алгоритм классификации "Парзеновское окно":** 
