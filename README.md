## Оглавление
1. <a href="#metr">Метрические алгоритмы классификации</a><br>
2. <a href="#bayes">Байесовские алгоритмы классификации</a><br>
3. <a href="#line">Линейные алгоритмы классификации</a><br>

# <a name="metr"></a> <center><b>Метрические алгоритмы</b></center>
<center>
<table>
  <tbody>
    <tr>
      <th>Метод</th>
      <th>Параметры</th>
      <th>Точность</th>
    </tr>
    <tr>
      <td><a href="#KWNN1">KWNN</a></td>
      <td>k=20, q=0.05</td>
      <td>0.02</td>
    </tr>
    <tr>
      <td><a href="#KNN1">KNN</a></td>
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
	<tr>
      <td><a href="#PF">PF</a></td>
      <td>in progress</td>
    </tr>
	<tr>
      <td><a href="#STOLP">STOLP</a></td>
      <td>in progress</td>
    </tr>
  </tbody>
   </table>
</center>
Гипотеза компактности - в задачах классификации предположение о том, что схожие объекты гораздо чаще лежат в одном классе,
чем в разных; или, другими словами, что классы образуют компактно локализованные подмножества в пространстве объектов.

Основываясь на данной гипотезе, реализуем алгоритм **k-ближайших соседей** на "Ирисах Фишера".

В реализованных методах выбрана евклидова метрика. 


```
 EucDist <- function(u, v) {
  sqrt(sum((u - v)^2))
}
```
## Алгоритм классификации "1NN"(добавлен в качестве дополнения):
![ONN](https://github.com/uhsd22/ML_LABS/blob/master/SBC/1NN/ONN.png)
### <a name="KNN1"></a> **1. Метод классификации k-ближайших соседей("kNN"):**

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

### <a name="KWNN1"></a>**2. Метод классификации k-взвешенных соседей("kwNN"):**  
Альтернативный вариант метода kNN: в каждом классе выбирается k ближайших объектов, и объект относится к тому классу, для
которого среднее расстояние до k ближайших соседей минимально.

В функции **LOOKWNN** подберем оптимальный параметр веса q при k = 6.
![LOOKWNN](https://github.com/uhsd22/Lab1/blob/master/LabIMG/KWNNLoo.png)

Для составления карты классификации параметр k равен 6, а параметр веса q равен 0.5:
![KWNN1](https://github.com/uhsd22/Lab1/blob/master/LabIMG/map_KWNNew.png)

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


## **4. Алгоритм классификации "Парзеновское окно":** 
Модель алгоритма:

1. Дана обучающая выборка xx, случайно выбранная точка z, ширина окна h.
2. Вычисляем расстояние от z до каждого объекта из выбоки xx.
3. Сортируем расстояния от минимума к максимуму.
4. Проходим по всех объектам выборки и опредлеяем вес при помощи функции ядра.


	![Parsen](https://github.com/uhsd22/ML_LABS/blob/master/LabIMG/parzen.jpg),где значение функции K определяется как расстояние от заданного z до всех объектов выборки деленное на ширину окна
5. Находим взвешенную сумму.

Реализация алгоритма
```
parsen <- function(xx, z, h, K){
  row <- dim(xx)[1]
  col <- dim(xx)[2]-1
  count_classes <- length(names(table(xx[,col+1])))
  classes <- rep(0,count_classes)
  names(classes) <- names(table(xx[,col+1]))
  for(i in 1:row){
    y <- xx[i,col+1]
    dist <- EucDist(xx[i,1:col],z)
    w <- K(dist/h)
    classes[y] <- classes[y] + w
  }
  if(sum(classes) > 0) class <- names(which.max(classes))
  else class <- "unknown"
  return(class)
}
```
Ширина окна h влияет на качество плотности. При h -> 0 плотность определяется вблизи обучающих объектов, а при h -> к бесконечности плотность сглаживается и вырождается в константу. Функция ядра K влияет на степень гладкости при вычислении парзеновского окна и эффективность вычислений.

**Ядра:**


**1. <a name="ParzenR"></a>Прямоугольное ядро**
```
kR <- function(z){
return (0.5 * (abs(z)<=1))
}
```
**Оптимальный параметр ширины окна h=0.35, оценка LOO=0.04. Карта классификации выглядит следующим образом:**
![MAP_RectKer](https://github.com/uhsd22/ML_LABS/blob/master/SBC/PW/RectKer.png)


**2. <a name="ParzenT"></a>Треугольное ядро**
```
kT <- function(z){
return ((1 - abs(z))*(abs(z)<=1))
}
```

**Оптимальный параметр ширины окна h=0.35, оценка LOO=0.04. Карта классификации выглядит следующим образом:**
![MAP_TrKer](https://github.com/uhsd22/ML_LABS/blob/master/SBC/PW/TrKer.png)


**3. <a name="ParzenQ"></a>Квартическое ядро**
```
kQ <- function(z){
return ((15/16)*(1 - z^2)^2 * (abs(z)<=1))
}
```

**Оптимальный параметр ширины окна h=0.35, оценка LOO=0.04. Карта классификации выглядит следующим образом:**
![MAP_QuadKer](https://github.com/uhsd22/ML_LABS/blob/master/SBC/PW/QuadKer.png)

**4. <a name="ParzenE"></a>Ядро Епанечникова**
```
kE <- function(z){
return ((3/4)*(1-z^2) * (abs(z)<=1))
}
```

**Оптимальный параметр ширины окна h=0.35, оценка LOO=0.04. Карта классификации выглядит следующим образом:**
![MAP_EpKer](https://github.com/uhsd22/ML_LABS/blob/master/SBC/PW/EpKer.png)


**5. <a name="ParzenG"></a>Ядро Гаусса**
```
kG <- function(z) {
  return ((2*pi)^(-0.5)*exp(-0.5*(z^2)))
}
```

**Оптимальный параметр ширины окна h=0.1, оценка LOO=0.04. Карта классификации выглядит следующим образом:**
![MAP_GaussKer](https://github.com/uhsd22/ML_LABS/blob/master/SBC/PW/GaussKer.png)

Применение гауссовского ядра в алгоритме парзеновского окна решает проблему, когда классифицируемая точка не попадает в окно из за малой ширины. Следовательно, можно классифицировать любые точки, не зависимо от их удаленности от выборки, чего нельзя сделать, использую другие функции ядер.


**Сравнение рассмотренных алгоритмов**
Метрические алгоритмы KWNN и парзеновского окна оба хорошо справляются с задачей классификации.
Для наглядности рассмотрим пример:

**KWNN**


![K](https://github.com/uhsd22/ML_LABS/blob/master/SBC/kNN/map_KWNNew.png)

**Ядро Гаусса**


![G](https://github.com/uhsd22/ML_LABS/blob/master/SBC/PW/GaussKerTmp.png)


***Плюсы и минусы парзеновского окна:***

- **Плюсы:**

	+ При правильно выбраном h алгоритм способен классифицировать объект с хорошим качеством;
	+ Алгоритм прост в реализации;
	+ Учитывются все точки с одинаковым расстоянием;
	+ Классификация происходит за O(N).


- **Минусы:**

	+ Параметр ширины окна требуется подбирать под конкретную обучающую выборку;
	+ Крайне малый набор параметров;
	+ Требуется хранить выборку целиком;
	+ Для всех ядер, кроме гауссовского, есть вероятность того, что точка не будет классифицированна из-за того, что она не попадает в окно.
	
	## **<a name="PF"></a>6. Метод потенциальных функций:** 

**Метод потенциальных функций** - относится к метрическим классификаторам. В отличии от метода парзеновских окон, окно строится вокруг обучающих точек.
При классификации объект проверяется на близость к объектам из обучающей выборки.

**Алгоритм:**
   - Изначально для каждого объекта выборки задаём *ширину окна* **h**.
   - Затем для обучающих объектов вычисляем *силу потенциала* **gamma**. 
   - После чего каждому объекту выборки присваивается *вес* по формуле ![](https://github.com/uhsd22/ML_LABS/blob/master/SBC/PF/pf.png), K(z) - функция ядра.
   - Суммируем веса объектов одинаковых классов. Класс с наибольшей суммой присваивается точке.
       ## **<a name="STOLP"></a>7. STOLP:** 

# <a name="bayes"></a> <center><b>Байесовские классификаторы</b></center>
Байесовский подход является классическим в теории распознавания образов
и лежит в основе многих методов. Он опирается на теорему о том, что если плотности
распределения классов известны, то алгоритм классификации, имеющий минимальную вероятность ошибок, можно выписать в явном виде.

**Ссылка на теорию**:[machinelearning](http://www.machinelearning.ru/wiki/images/6/63/voron-ml-bayes1.pdf)
   ## **1. Линии уровня нормального распределения:**
   
   Нормальный дискриминантный анализ — это специальный случай байесовской
классификации, когда предполагается, что плотности всех классов являются многомерными нормальными. Этот случай интересен и удобен тем, что задача оценивания параметров распределения по выборке решается аналитически.

Пусть ![q](https://github.com/uhsd22/ML_LABS/blob/master/LabIMG/line1.png), то есть объекты описываются n числовыми признаками.

Вероятностное распределение с плотностью ![qq](https://github.com/uhsd22/ML_LABS/blob/master/LabIMG/line2.png)

называется n-мерным нормальным (гауссовским) распределением с математическим
ожиданием (центром) ![qqq](https://github.com/uhsd22/ML_LABS/blob/master/LabIMG/line3.png) и ковариационной матрицей ![qqqq](https://github.com/uhsd22/ML_LABS/blob/master/LabIMG/line4.png). Предполагается,что матрица Σ симметричная, невырожденная, положительно определённая.


[Здесь ссылка на вывод формулы из конспекта](https://raw.githubusercontent.com/uhsd22/ML_LABS/master/LabIMG/form_lines.jpg)


**Код:**


```
countt <- function( x) { #плотность нормального многомерного распределения
  
  tmp <- (exp((t(x - M) %*% solve(Sigma) %*% (x - M))/(-2)))/(sqrt(det(Sigma) * (2 * pi)^dim(Sigma)[1]))

  return(tmp)
}
M <- c(5, 5)
left_x <- M[1] - 8
right_x <- M[1] + 8
bot_y <- M[2] - 8
up_y <- M[2] + 8


x <- seq(left_x, right_x, 0.05)
y <- seq(bot_y, up_y, 0.05)

par(bg = 'cadetblue', fg = 'white')
plot(left_x:right_x, bot_y:up_y,xlab = "x", ylab = "y", type = "n")


for (i in x) {
  
  for (j in y) {
    
    color <- adjustcolor("blue", countt(c(i, j))) #оттенки цвета
    points(i, j, pch = 22,col = color, bg = color)
    
  }
  
}

 z = outer(x, y, function(x, y) {
   unlist(lapply(1:length(x), function(i) countt(c(x[i], y[i])))) #двумерный вектор значений
   
 })
 #print(z)
 
 
 Sigma <- matrix(NA, 2, 2)
 # #matrix 1
 # Sigma[1, 1] <- 3
 # Sigma[2, 2] <- 3
 # Sigma[1, 2] <- 0
 # Sigma[2, 1] <- 0
 # #matrix 2
 #  Sigma[1, 1] <- 1
 #  Sigma[2, 2] <- 5
 #  Sigma[1, 2] <- 0
 #  Sigma[2, 1] <- 0
  # #matrix 3
  # Sigma[1, 1] <- 5
  # Sigma[2, 2] <- 1
  # Sigma[1, 2] <- 0
  # Sigma[2, 1] <- 0
  # #matrix 4
  # Sigma[1, 1] <- 3
  # Sigma[2, 2] <- 9
  # Sigma[1, 2] <- 3
  # Sigma[2, 1] <- 3
 #matrix 5
 Sigma[1, 1] <- 9
 Sigma[2, 2] <- 3
 Sigma[1, 2] <- 3
 Sigma[2, 1] <- 3
 contour(x,y,z,add = T)
 z <- 0
```

   - Если признаки имеют одинаковые дисперсии , то эллипсоиды являются сферами.
<table>
	<td>
      <img src="https://github.com/uhsd22/ML_LABS/blob/master/Bayes/lev_lines/circle_ex.png" width="500" heigth="200">
    </td>
</table>

   - Если признаки некоррелированы,  то линии уровня плотности распределения имеют форму эллипсоидов с осями, параллельными осям координат.
   <table>
	<tr>
    <td>
      <img src="https://github.com/uhsd22/ML_LABS/blob/master/Bayes/lev_lines/norm_ellipse(1_5).png" width="500" heigth="200">
    </td>
    <td>
      <img src="https://github.com/uhsd22/ML_LABS/blob/master/Bayes/lev_lines/norm_ellipse2(5_1).png" width="500" heigth="200">
    </td>
	</tr>
	<table>
		
   - Если признаки коррелированы, то матрица Σ не диагональна и линии уровня имеют форму эллипсоидов, оси которых повернуты (направлены вдоль собственных векторов матрицы Σ) относительно исходной системы координат.
   <table>
  <tr>
    <td>
      <img src="https://github.com/uhsd22/ML_LABS/blob/master/Bayes/lev_lines/deform_ellipse(9_3).png" width="500" heigth="200">
    </td>
    <td>
      <img src="https://github.com/uhsd22/ML_LABS/blob/master/Bayes/lev_lines/deform_ellipse2(3_9).png" width="500" heigth="200">
    </td>
  </tr>
</table>

   # <a name="metr"></a> <center><b>Линейные алгоритмы класификации</b></center>
