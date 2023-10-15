/* 이원배치법 자료입력 */
data data;
  input way temperature lifespan;
  cards;
1 700 35
1 700 39
1 700 34
1 800 40
1 800 46
1 800 31
1 900 42
1 900 43
1 900 38
1 1000 41
1 1000 47
1 1000 41
2 700 33
2 700 36
2 700 36
2 800 37
2 800 41
2 800 35
2 900 45
2 900 47
2 900 37
2 1000 46
2 1000 45
2 1000 45
3 700 36
3 700 40
3 700 37
3 800 42 
3 800 45
3 800 39
3 900 46
3 900 44
3 900 44
3 1000 45
3 1000 49
3 1000 50
;
run;

* 교호작용항 유의성 검정 ;
proc glm data = data;
  class way temperature;
  model lifespan = way temperature way*temperature;
  random temperature way*temperature /test;
run;

* 방법=고정, 온도=랜덤 ;  
proc glm data = data;
  class way temperature;
  model lifespan = way temperature;
  random temperature /test;
run;

/* 이원배치법 잔차분석 */
* 정규성 ;
proc glm data = data;
  class way temperature;
  model lifespan = way temperature;
  random temperature /test;
  output out = test p = pred r = res;
run;

proc univariate data = test normal noprint;
var res;
qqplot res / normal (L=1 mu=est sigma=est);
histogram res / normal;
run;

* 등분산성 검정 ;
data data2;
input way temperature lifespan rep;
trt = catx('-',way,temperature);
cards;
1 700 35 1
1 700 39 2
1 700 34 3
1 800 40 1
1 800 46 2
1 800 31 3
1 900 42 1
1 900 43 2
1 900 38 3
1 1000 41 1
1 1000 47 2
1 1000 41 3
2 700 33 1
2 700 36 2
2 700 36 3
2 800 37 1
2 800 41 2
2 800 35 3
2 900 45 1
2 900 47 2
2 900 37 3
2 1000 46 1
2 1000 45 2
2 1000 45 3
3 700 36 1
3 700 40 2
3 700 37 3
3 800 42 1
3 800 45 2
3 800 39 3
3 900 46 1
3 900 44 2
3 900 44 3
3 1000 45 1
3 1000 49 2
3 1000 50 3
;

proc glm data = data2;
class way temperature;
model lifespan = way temperature;
output out=test p=pred r=res;

proc glm data = test;
class trt;
model res = trt;
means trt / hovtest = bartlett hovtest = levene hovtest = bf;

* 독립성 ;
proc gplot data = test;
plot res*pred;
run;


/* 이원배치법 다중비교 */
* 1) LSD ;
proc glm data = data;
  class way temperature;
  model lifespan = way temperature;
  random temperature /test;
  means way temperature / lsd lines;  
run;

* 2) 본페로니 수정 ;
proc glm data = data;
  class way temperature;
  model lifespan = way temperature;
  random temperature /test;
  means way temperature / bon lines;  
run;

* 3) 튜키 ;
proc glm data = data;
  class way temperature;
  model lifespan = way temperature;
  random temperature /test;
  lsmeans way / pdiff = all adjust = tukey slice = way;
  lsmeans temperature / pdiff = all adjust = tukey slice = temperature; 
run;

proc glm data = data;
  class way temperature;
  model lifespan = way temperature;
  random temperature /test;
  means way temperature / tukey;
run;


/* 분할구 자료입력 */
data data3;
  input  way $ temperature $ rep $ lifespan;
  cards;
1 700 1 35
1 700 2 39
1 700 3 34
1 800 1 40
1 800 2 46
1 800 3 31
1 900 1 42
1 900 2 43
1 900 3 38
1 1000 1 41
1 1000 2 47
1 1000 3 41
2 700 1 33
2 700 2 36
2 700 3 36
2 800 1 37
2 800 2 41
2 800 3 35
2 900 1 45
2 900 2 47
2 900 3 37
2 1000 1 46 
2 1000 2 45
2 1000 3 45
3 700 1 36
3 700 2 40
3 700 3 37
3 800 1 42
3 800 2 45
3 800 3 39
3 900 1 46
3 900 2 44
3 900 3 44
3 1000 1 45
3 1000 2 49
3 1000 3 50
;

proc glm data = data3;
class rep way temperature;
model lifespan = rep way rep*way temperature;
random rep rep*way / test;
run;

/* 분할구 잔차분석 */
proc glm data = data3;
class rep way temperature;
model lifespan = rep way rep*way temperature;
random rep rep*way / test;
output out=test2 p=pred r=res;

* 정규성 ;
proc univariate data = test2 normal noprint;
var res;
qqplot res / normal (L=1 mu=est sigma=est);
histogram res / normal;
run;

* 등분산성 ;
proc glm data = test2;
class trt;
model res = trt;
means trt / hovtest = bartlett hovtest = levene hovtest = bf;

* 독립성 ;
proc gplot;
plot res*pred;
run;

/* 분할구 다중비교 */
* 1) LSD ;
proc glm data = data3;
  class rep way temperature;
  model lifespan = rep way rep*way temperature;
  random rep rep*way / test;
  means way temperature / lsd lines;  
run;

* 2) 본페로니 수정 ;
proc glm data = data3;
  class rep way temperature;
  model lifespan = rep way rep*way temperature;
  random rep rep*way / test;
  means way temperature / bon lines;  
run;

* 3) 튜키 ;
proc glm data = data3;
  class rep way temperature;
  model lifespan = rep way rep*way temperature;
  random rep rep*way / test;
  lsmeans way / pdiff = all adjust = tukey slice = way;
  lsmeans temperature / pdiff = all adjust = tukey slice = temperature; 
run;