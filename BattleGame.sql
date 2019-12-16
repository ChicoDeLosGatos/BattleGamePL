/******************************************************************************************************************

	Primero comenzaremos creando el tipo de dato que usaremos para nuestros personajes, concretamente
	el tipo de dato BATTLE_CHARACTER.

*****************************************************************************************************************/

CREATE OR REPLACE TYPE BATTLE_CHARACTER AS OBJECT(
chr_id   DECIMAL,
chr_name VARCHAR2(150),
chr_hp DECIMAL,
chr_str DECIMAL,
chr_spd DECIMAL,
chr_def DECIMAL,
chr_lck DECIMAL,
chr_wpn DECIMAL,
chr_lvl DECIMAL,
chr_hbl VARCHAR2(150),
chr_dmn DECIMAL,
CONSTRUCTOR FUNCTION BATTLE_CHARACTER(
   param_id  DECIMAL,
   param_name   VARCHAR2
) RETURN SELF AS RESULT,
CONSTRUCTOR FUNCTION BATTLE_CHARACTER(
   param_id  DECIMAL,
   param_name   VARCHAR2,
   param_hp DECIMAL,
   param_str DECIMAL,
   param_spd DECIMAL,
   param_def DECIMAL
) RETURN SELF AS RESULT,
MEMBER PROCEDURE getStats,
MEMBER PROCEDURE attack(chr_target BATTLE_CHARACTER),
MEMBER PROCEDURE rand_attack,
MEMBER PROCEDURE chg_hp(chngs DECIMAL),
MEMBER PROCEDURE lvl_up,
MEMBER PROCEDURE quit_hbl,
MEMBER PROCEDURE damnation(damnator BATTLE_CHARACTER),
MEMBER PROCEDURE assassination(assassin BATTLE_CHARACTER),
MEMBER PROCEDURE invocation(sorcerer BATTLE_CHARACTER),
MEMBER PROCEDURE heal,
MEMBER FUNCTION get_hp RETURN DECIMAL,
MEMBER FUNCTION show_hp RETURN VARCHAR2,
MEMBER FUNCTION get_str RETURN DECIMAL,
MEMBER FUNCTION get_spd RETURN DECIMAL,
MEMBER FUNCTION get_def RETURN DECIMAL,
MEMBER FUNCTION get_lck RETURN DECIMAL,
MEMBER FUNCTION get_lvl RETURN DECIMAL,
MEMBER FUNCTION get_name RETURN VARCHAR2,
MEMBER FUNCTION get_hbl RETURN VARCHAR2,
MEMBER FUNCTION try_surr RETURN BOOLEAN
);


CREATE OR REPLACE TYPE BODY BATTLE_CHARACTER AS
   CONSTRUCTOR FUNCTION BATTLE_CHARACTER(
      param_id  DECIMAL,
      param_name   VARCHAR2
   ) RETURN SELF AS RESULT IS
   total_points DECIMAL(3);
   habilities LIST;
   BEGIN
      total_points := 10;
      self.chr_lck := dbms_random.value(1,10);
      total_points := total_points - dbms_random.value(1,3);
      self.chr_str := total_points;
      total_points := total_points - dbms_random.value(1,3);
      self.chr_spd := total_points;
      total_points := total_points - dbms_random.value(1,3);
      self.chr_def := total_points;
      self.chr_id := param_id;
      self.chr_name := param_name;
      self.chr_hp := (total_points*100/(chr_lck/2))*1.3;
      self.chr_lvl := 0;
      IF self.chr_hp > 1000 THEN self.chr_hp := self.chr_hp * 0.9; 
      END IF;
      self.chr_wpn := dbms_random.value(50, 100);
      self.chr_dmn := 0;
      RETURN;
   END;
   
   CONSTRUCTOR FUNCTION BATTLE_CHARACTER(
   param_id  DECIMAL,
   param_name   VARCHAR2,
   param_hp DECIMAL,
   param_str DECIMAL,
   param_spd DECIMAL,
   param_def DECIMAL
) RETURN SELF AS RESULT IS
BEGIN
      self.chr_id := param_id;
      self.chr_name := param_name;
      self.chr_hp := param_hp*10;
      self.chr_str := param_str;
      self.chr_spd := param_spd;
      self.chr_def := param_def;
      self.chr_lck := dbms_random.value(1,10);
      self.chr_wpn := dbms_random.value(50, 100);
      IF chr_hp < 100 THEN
      self.chr_str := chr_str * 25/dbms_random.value(1, chr_str);
      END IF;
      self.chr_lvl := 0;
      self.chr_dmn := 0;
      RETURN;
END;

MEMBER PROCEDURE getStats IS
habilities LIST;
bonus DECIMAL(3,1);
wpns LIST;
wpn VARCHAR2(200);
BEGIN
wpns := SPLIT('Espada, Hacha, Cuchillo, Cadenas, Espada doble, Espadon');
wpn := wpns(dbms_random.value(1, wpns.count));
bonus := 1;
CASE wpn
   WHEN 'Espada' THEN self.chr_wpn:= self.chr_wpn*1;
   WHEN 'Hacha' THEN self.chr_wpn:= self.chr_wpn*1.2;
   WHEN 'Cuchillo' THEN self.chr_wpn:= self.chr_wpn*0.2;
   WHEN 'Cadenas' THEN self.chr_wpn:= self.chr_wpn*0.6;
   WHEN 'Espada doble' THEN self.chr_wpn:= self.chr_wpn*1.5;
   WHEN 'Espadon' THEN self.chr_wpn:= self.chr_wpn*2;
END CASE;
habilities := SPLIT('Curacion, Maldicion, Invocacion, Resurreccion, Asesinato');
self.chr_hbl := habilities(dbms_random.value(1,habilities.count));
IF chr_hp < 100 THEN
   bonus := 100/dbms_random.value(1, chr_str);
   DBMS_OUTPUT.PUT_LINE(
   'Nombre: '||self.chr_name||CHR(10)||
   'HP: '||self.chr_hp||CHR(10)||
   'STR: '||self.chr_str||CHR(10)||
   'DEF: '|| self.chr_def || CHR(10)||
   'SPD: '||self.chr_spd || CHR(10)||
   'ARMA: '||wpn||' (+'||self.chr_wpn||' ATK)'|| CHR(10)||
   'HABILIDAD: '||self.chr_hbl||CHR(10)||
   'LCK: '||self.chr_lck || CHR(10)||
   'LVL: '||self.chr_lvl||CHR(10)||
   'BONUS: '||bonus||'%'||CHR(10));
   ELSE
    DBMS_OUTPUT.PUT_LINE(
   'Nombre: '||self.chr_name||CHR(10)||
   'HP: '||self.chr_hp||CHR(10)||
   'STR: '||self.chr_str||CHR(10)||
   'DEF: '|| self.chr_def || CHR(10)||
   'SPD: '||self.chr_spd || CHR(10)||
   'ARMA: '||wpn||' (+'||self.chr_wpn||' ATK)'|| CHR(10)||
   'HABILIDAD: '||self.chr_hbl||CHR(10)||
   'LCK: '||self.chr_lck || CHR(10)||
   'LVL: '||self.chr_lvl||CHR(10));
   END IF;
   chr_str := chr_str * bonus;
END;

MEMBER PROCEDURE attack(chr_target BATTLE_CHARACTER) IS
dmg DECIMAL(6, 1);
lck_p DECIMAL(3);
damage DECIMAL(5);
BEGIN
   damage := chr_target.get_str();
   lck_p := dbms_random.value(1,12-chr_target.get_lck());
   IF lck_p = 2 THEN
   dmg := damage/self.get_def() + (dbms_random.value(1, chr_target.get_str()) * 2) * 
   dbms_random.value(1,self.get_str())/dbms_random.value(1,50) + (chr_target.get_lck()/self.get_lck())*
   10/dbms_random.value(1,dbms_random.value(1, chr_target.get_lck()*10))+
   dbms_random.value(chr_target.get_lck(), chr_target.get_lck()*2) * (chr_target.get_lck()/2) + chr_target.chr_wpn/2;
   ELSE
   dmg := damage/self.get_def() + (dbms_random.value(1, chr_target.get_str()) * 2) * 
   dbms_random.value(1,self.get_str())/dbms_random.value(1,50) - (chr_target.get_lck()/self.get_lck())*
   10/dbms_random.value(1,dbms_random.value(1, chr_target.get_lck()*10))+
   dbms_random.value(chr_target.get_lck(), chr_target.get_lck*2) + chr_target.chr_wpn/2;   
   END IF;
   IF(dmg < 0) THEN dmg := 0; END IF;
   self.chg_hp(self.get_hp() - dmg);
   IF lck_p = 2 THEN DBMS_OUTPUT.PUT_LINE('Ataque crítico!'); END IF;
   IF chr_target.get_hp < 0 THEN 
      DBMS_OUTPUT.PUT_LINE(self.chr_name || ' ha sido atacado por '|| chr_target.get_name()|| ' antes de morir y ha recibido ' || dmg || ' puntos de daño.');
   ELSE
   DBMS_OUTPUT.PUT_LINE(self.chr_name || ' ha sido atacado por '|| chr_target.get_name()|| ' y ha recibido ' || dmg || ' puntos de daño.');
   END IF;
   DBMS_OUTPUT.PUT_LINE(self.chr_name|| ' => ' || self.show_hp() || ' HP.' || CHR(10) || 
   chr_target.chr_name|| ' => ' || chr_target.show_hp() || ' HP.');
   DBMS_OUTPUT.PUT_LINE('/*****************************************************************************/');
   IF(self.chr_dmn > 0) THEN
   DBMS_OUTPUT.PUT_LINE(get_name()||' es víctima de una maldición.'||CHR(10)||get_name()||' ha perdido '||self.chr_dmn||' HP.');
   chg_hp(get_hp()-self.chr_dmn);
   DBMS_OUTPUT.PUT_LINE(get_name()||' => '||get_hp()||' HP.');
      DBMS_OUTPUT.PUT_LINE('/*****************************************************************************/');
END IF;
END;

MEMBER PROCEDURE lvl_up IS
dmg DECIMAL(6, 1);
lck_p DECIMAL(3);
damage DECIMAL(5);
BEGIN
   self.chr_lvl := self.chr_lvl + 1;
   self.chr_hp := self.chr_hp + 100*self.chr_lvl;
   self.chr_str := self.chr_str + 2*self.chr_lvl;
   self.chr_def := self.chr_def + 2*self.chr_lvl;
   self.chr_spd := self.chr_spd + 2*self.chr_lvl;
IF(chr_lvl > 1) THEN
  DBMS_OUTPUT.PUT_LINE('/*****************************************************************************/');
  DBMS_OUTPUT.PUT_LINE(self.get_name() || ' ha subido de nivel! Ahora es nivel '||self.get_lvl());
   DBMS_OUTPUT.PUT_LINE('/*****************************************************************************/');
END IF;
END;



MEMBER PROCEDURE rand_attack IS
dmg DECIMAL(10);
lck_p DECIMAL(3);
damage DECIMAL(10);
names LIST;
chosen_name VARCHAR(400);
chances NUMBER(3);
pos NUMBER(3);
BEGIN
   chances := self.chr_lvl;
   pos := 0;
   names := SPLIT('un jabalí, un golem, un desastre natural, un espectro, un goblin, un orco, un lobo, un cuervo, un bandido, una quimera, un dragon');
   damage := dbms_random.value(15000*self.chr_lvl, 30000*self.chr_lvl-(self.chr_lvl-1)*10*(1000*self.chr_lck));
   LOOP
   lck_p := dbms_random.value(1,10);
      IF lck_p = 3 OR lck_p = 7
         THEN
            dmg := DAMAGE*self.chr_lvl*0.69;
            chosen_name:= names(dbms_random.value(1, names.count));
               CASE chosen_name
                  WHEN 'un jabalí' THEN dmg := dmg / 500*(5-self.chr_lvl) - 100*dbms_random.value(1, self.chr_lck);
                  WHEN 'un golem' THEN dmg := dmg / 70*(5-self.chr_lvl) - 100*dbms_random.value(1, self.chr_lck); 
                  WHEN 'un desastre natural' THEN dmg := dmg / 50*(5-self.chr_lvl) - 100*dbms_random.value(1, self.chr_lck);
                  WHEN 'un espectro' THEN dmg := dmg / 700*(5-self.chr_lvl) - 100*dbms_random.value(1, self.chr_lck);
                  WHEN 'un goblin' THEN dmg := dmg / 400*(5-self.chr_lvl) - 100*dbms_random.value(1, self.chr_lck);
                  WHEN 'un orco' THEN dmg := dmg / 350*(5-self.chr_lvl) - 100*dbms_random.value(1, self.chr_lck);
                  WHEN 'un lobo' THEN dmg := dmg / 450*(5-self.chr_lvl) - 100*dbms_random.value(1, self.chr_lck);
                  WHEN 'un cuervo' THEN dmg := dmg / 800*(5-self.chr_lvl) - 100*dbms_random.value(1, self.chr_lck);
                  WHEN 'un bandido' THEN dmg := dmg / 300*(5-self.chr_lvl) - 100*dbms_random.value(1, self.chr_lck);
                  WHEN 'una quimera' THEN dmg := dmg / 250*(5-self.chr_lvl) - 100*dbms_random.value(1, self.chr_lck);
                  WHEN 'un dragon' THEN dmg := dmg / 30*(5-self.chr_lvl) - 100*dbms_random.value(1, self.chr_lck);
                  END CASE;
                  IF(dmg < 0) THEN 
                  
                  dmg := dbms_random.value(1, self.chr_hp/self.chr_lck); 
                  
                  END IF;
   self.chg_hp(self.get_hp() - dmg);
   IF lck_p = 2 THEN DBMS_OUTPUT.PUT_LINE('Ataque crítico!'); 
   END IF;
   DBMS_OUTPUT.PUT_LINE(self.chr_name || ' ha sido atacado por '|| chosen_name || ' y ha recibido ' || dmg || 
   ' puntos de daño.');
   DBMS_OUTPUT.PUT_LINE(self.chr_name|| ' => ' || self.show_hp() || ' HP.');
   DBMS_OUTPUT.PUT_LINE('/*****************************************************************************/');
   END IF;
   pos := pos+1;
   EXIT WHEN lck_p = 5 OR pos = chances;
   END LOOP;   
END;

MEMBER PROCEDURE assassination(assassin BATTLE_CHARACTER) IS
rnd NUMBER(3);
BEGIN
rnd := dbms_random.value(1, 50);
IF rnd = 25 
THEN
   DBMS_OUTPUT.PUT_LINE(self.get_name||' acepta las paces pero cuando deja el arma '||assassin.get_name || ' saca un puñal y se lo ensarta,'||CHR(10)||' matando a '||self.get_name||'.');
   self.chg_hp(0);
   ELSE
   DBMS_OUTPUT.PUT_LINE('Pero '||self.get_name||' descubre la trampa de '||assassin.get_name||' y no cae en ella.');
   END IF;
      DBMS_OUTPUT.PUT_LINE('/*****************************************************************************/');

END;

MEMBER PROCEDURE heal IS
rnd NUMBER(3);
plus_hp NUMBER(5);
BEGIN
rnd := DBMS_RANDOM.VALUE(1, 5);
IF rnd = 3 THEN
plus_hp := DBMS_RANDOM.VALUE(20, 1000);
chg_hp(get_hp()+plus_hp);
DBMS_OUTPUT.PUT_LINE(self.get_name()||' ha curado sus heridas y tiene +'||plus_hp||' HP.'||
CHR(10)||self.get_name()||' => '||get_hp()||'.');
   DBMS_OUTPUT.PUT_LINE('/*****************************************************************************/');

END IF;

END;

MEMBER PROCEDURE damnation(damnator BATTLE_CHARACTER) IS
rnd NUMBER(3);
potential DECIMAL;
BEGIN
rnd := DBMS_RANDOM.VALUE(1, 30);
IF rnd = 15 
THEN
DBMS_OUTPUT.PUT_LINE(damnator.get_name()||' ha lanzado una maldición.');
DBMS_OUTPUT.PUT_LINE(self.get_name()|| ' ha sido maldito.');
potential := DBMS_RANDOM.VALUE(1, 50);
chr_dmn := potential;
DBMS_OUTPUT.PUT_LINE('/*****************************************************************************/');
END IF;
END;

MEMBER PROCEDURE invocation(sorcerer BATTLE_CHARACTER) IS
dmg DECIMAL(10); 
lck_p DECIMAL(3);
damage DECIMAL(10);
names LIST;
chosen_name VARCHAR(400);
chances NUMBER(3);
pos NUMBER(3);
BEGIN
   chances := self.chr_lvl;
   pos := 0;
   names := SPLIT('un jabalí, un golem, un desastre natural, un espectro, un goblin, un orco, un lobo, un cuervo, un bandido, una quimera, un dragon');
   damage := dbms_random.value(5000, 25000);
   LOOP
   lck_p := dbms_random.value(1,6);
      IF lck_p = 5
         THEN
            dmg := DAMAGE*self.chr_lvl*0.69;
            chosen_name:= names(dbms_random.value(1, names.count));
               CASE chosen_name
                  WHEN 'un jabalí' THEN dmg := dmg / 500;--* dbms_random.value(1, 500);
                  WHEN 'un golem' THEN dmg := dmg / 70;--* dbms_random.value(1, 20);
                  WHEN 'un desastre natural' THEN dmg := dmg / 50;--* dbms_random.value(1, 50);
                  WHEN 'un espectro' THEN dmg := dmg / 700;--* dbms_random.value(1, 700);
                  WHEN 'un goblin' THEN dmg := dmg / 400;--* dbms_random.value(1, 400);
                  WHEN 'un orco' THEN dmg := dmg / 350;--* dbms_random.value(1, 350);
                  WHEN 'un lobo' THEN dmg := dmg / 450;--* dbms_random.value(1, 450);
                  WHEN 'un cuervo' THEN dmg := dmg / 800;--* dbms_random.value(1, 800);
                  WHEN 'un bandido' THEN dmg := dmg / 300;--* dbms_random.value(1, 300);
                  WHEN 'una quimera' THEN dmg := dmg / 250;--* dbms_random.value(1, 250);
                  WHEN 'un dragon' THEN dmg := dmg / 30;
               END CASE;
   IF(dmg < 0) THEN dmg := 0; 
   END IF;
   self.chg_hp(self.get_hp() - dmg);
   IF lck_p = 2 THEN DBMS_OUTPUT.PUT_LINE('Ataque crítico!'); 
   END IF;
   DBMS_OUTPUT.PUT_LINE(sorcerer.chr_name || ' ha invocado '|| chosen_name || ' y ha atacado a ' || self.chr_name || ' con ' || dmg || ' puntos de daño.');
   DBMS_OUTPUT.PUT_LINE(self.chr_name|| ' => ' || self.show_hp() || ' HP.');
   DBMS_OUTPUT.PUT_LINE('/*****************************************************************************/');
   END IF;
   pos := pos+1;
   EXIT WHEN lck_p = 5 OR pos = chances;
   END LOOP;  
END;

MEMBER PROCEDURE chg_hp(chngs DECIMAL) IS
hp DECIMAL(4,1);
BEGIN
   self.chr_hp := chngs;
END;

MEMBER PROCEDURE quit_hbl IS
BEGIN
   self.chr_hbl := 'Ninguna';
END;

MEMBER FUNCTION get_hp RETURN DECIMAL IS
BEGIN
    RETURN self.chr_hp;

END;

MEMBER FUNCTION show_hp RETURN VARCHAR2 IS
BEGIN
   IF self.chr_hp < 1 AND self.chr_hp > -1 THEN
      IF self.chr_hp < 0
         THEN
            RETURN '-0'||SQRT(POWER(chr_hp, 2));
         ELSE
            RETURN '0'||chr_hp;
         END IF;
   ELSE
      RETURN self.chr_hp||'';
      END IF;
END;

MEMBER FUNCTION get_spd RETURN DECIMAL IS
BEGIN
    RETURN self.chr_str;
END;

MEMBER FUNCTION get_str RETURN DECIMAL IS
BEGIN
   RETURN self.chr_str;
END;

MEMBER FUNCTION get_def RETURN DECIMAL IS
BEGIN
   RETURN self.chr_def;
END;

MEMBER FUNCTION get_lck RETURN DECIMAL IS
BEGIN
   RETURN self.chr_lck;
END;

MEMBER FUNCTION get_name RETURN VARCHAR2 IS
BEGIN
   RETURN self.chr_name;
END;

MEMBER FUNCTION get_hbl RETURN VARCHAR2 IS
BEGIN
   RETURN self.chr_hbl;
END;

MEMBER FUNCTION get_lvl RETURN DECIMAL IS
BEGIN
   RETURN self.chr_lvl;
END;

MEMBER FUNCTION try_surr RETURN BOOLEAN IS
n_rand NUMBER(4);
BEGIN
   n_rand := DBMS_RANDOM.VALUE(1, 12);
   IF(n_rand = 10) THEN
 DBMS_OUTPUT.PUT_LINE(self.chr_name|| ' ha intentado hacer las paces.');
   DBMS_OUTPUT.PUT_LINE('/*****************************************************************************/');
   RETURN TRUE;
   ELSE
    DBMS_OUTPUT.PUT_LINE(self.chr_name|| ' ha mirado a su oponente con desprecio.');
   DBMS_OUTPUT.PUT_LINE('/*****************************************************************************/');
   RETURN FALSE;
   END IF;
   END;
END;

/*************************************************************************************************

	Una vez nuestro tipo de dato está creado, creamos nuestros personajes y 
	jugamos el juego.

************************************************************************************************/

CREATE OR REPLACE PROCEDURE MODULED_BATTLE_GAME(
PARAM_CHARACTERS IN VARCHAR2
) AS
var_mc_player_one BATTLE_CHARACTER;
var_mc_player_two BATTLE_CHARACTER;
var_b_surrpla1 BOOLEAN;
var_b_surrpla2 BOOLEAN;
var_list_players LIST;
var_n_pos NUMBER(4);
var_n_rand NUMBER(5);

PROCEDURE procedure_module_battle IS
   BEGIN
   WHILE var_mc_player_one.get_hp() > 0 AND var_mc_player_two.get_hp > 0
   LOOP
   
   
  
  var_mc_player_one.rand_attack();
  var_mc_player_two.rand_attack();
  var_n_rand := DBMS_RANDOM.VALUE(1, 10);
  IF(var_n_rand = 5) THEN
  var_b_surrpla1 := var_mc_player_one.try_surr();
  var_b_surrpla2 := var_mc_player_two.try_surr();
      IF(var_b_surrpla1 = TRUE AND var_b_surrpla2 = TRUE) THEN
         DBMS_OUTPUT.PUT_LINE('Ambos jugadores han hecho las paces y han abandonado la pelea');
         var_mc_player_one := NULL;
         var_mc_player_two := NULL;
      END IF;
      EXIT WHEN var_b_surrpla1 = TRUE AND var_b_surrpla2 = TRUE;
   ELSE

      IF var_mc_player_two.get_spd() > var_mc_player_one.get_spd()
         THEN
            var_mc_player_one.attack(var_mc_player_two);
            var_mc_player_two.attack(var_mc_player_one);
         ELSE
            var_mc_player_two.attack(var_mc_player_one);
            var_mc_player_one.attack(var_mc_player_two);
      END IF;
      
      CASE
       WHEN var_mc_player_one.get_hbl() = 'Resurreccion' AND var_mc_player_one.get_hp() <= 0 THEN
      var_mc_player_one.chg_hp(dbms_random.value(100, 500));
      DBMS_OUTPUT.PUT_LINE(var_mc_player_one.get_name()|| ' ha resucitado!!'||CHR(10)||var_mc_player_one.get_name()||' => '||var_mc_player_one.get_hp()||' HP.');
      DBMS_OUTPUT.PUT_LINE('/*****************************************************************************/');

      var_mc_player_one.quit_hbl();
  
     WHEN var_mc_player_two.get_hbl() = 'Resurreccion' AND var_mc_player_two.get_hp() <= 0 THEN
      var_mc_player_two.chg_hp(dbms_random.value(100, 500));
      DBMS_OUTPUT.PUT_LINE(var_mc_player_two.get_name()|| ' ha resucitado!!'||CHR(10)||var_mc_player_two.get_name()||' => '||var_mc_player_two.get_hp()||' HP.');
      DBMS_OUTPUT.PUT_LINE('/*****************************************************************************/');
      var_mc_player_two.quit_hbl();
      
       WHEN var_mc_player_one.get_hbl() = 'Curacion' THEN
      var_mc_player_one.heal();
  
      WHEN var_mc_player_two.get_hbl() = 'Curacion' THEN
      var_mc_player_two.heal();
      
      WHEN var_mc_player_one.get_hbl() = 'Maldicion' THEN
      var_mc_player_two.damnation(var_mc_player_one);
  
      WHEN var_mc_player_two.get_hbl() = 'Maldicion' THEN
      var_mc_player_one.damnation(var_mc_player_two);
      
      WHEN var_mc_player_one.get_hbl() = 'Invocacion' THEN
      var_mc_player_two.invocation(var_mc_player_one);
  
      WHEN var_mc_player_two.get_hbl() = 'Invocacion' THEN
      var_mc_player_one.invocation(var_mc_player_two);
      
       WHEN var_mc_player_one.get_hbl() = 'Asesinato' THEN
       DBMS_OUTPUT.PUT_LINE(var_mc_player_one.get_name||' ha intentado hacer las paces.');
             DBMS_OUTPUT.PUT_LINE('/*****************************************************************************/');

      var_mc_player_two.assassination(var_mc_player_one);
  
      WHEN var_mc_player_two.get_hbl() = 'Asesinato' THEN
      DBMS_OUTPUT.PUT_LINE(var_mc_player_two.get_name||' ha intentado hacer las paces.');
            DBMS_OUTPUT.PUT_LINE('/*****************************************************************************/');

      var_mc_player_one.assassination(var_mc_player_two);
      
      ELSE     
      var_mc_player_one:=var_mc_player_one;
   END CASE;
      
      END IF;
   END LOOP;

   CASE
    WHEN var_mc_player_one.get_hp > 0 AND var_mc_player_two.get_hp <= 0 AND var_mc_player_one IS NOT NULL AND var_mc_player_two IS NOT NULL
      THEN
         DBMS_OUTPUT.PUT_LINE('La victoria es para '||var_mc_player_one.get_name()||'!!');
         var_mc_player_one.lvl_up();
         var_mc_player_two := NULL;
    WHEN var_mc_player_two.get_hp > 0 AND var_mc_player_one.get_hp <= 0 AND var_mc_player_one IS NOT NULL AND var_mc_player_two IS NOT NULL
      THEN
         DBMS_OUTPUT.PUT_LINE('La victoria es para '||var_mc_player_two.get_name()||'!!');
         var_mc_player_one := NULL;
         var_mc_player_two.lvl_up();
      ELSE
         IF var_mc_player_one IS NULL AND var_mc_player_two IS NOT NULL THEN
                  DBMS_OUTPUT.PUT_LINE('La victoria es para '||var_mc_player_two.get_name()||'!!');
         ELSIF var_mc_player_two IS NULL AND var_mc_player_one IS NOT NULL THEN
         DBMS_OUTPUT.PUT_LINE('La victoria es para '||var_mc_player_one.get_name()||'!!');
         ELSE
          DBMS_OUTPUT.PUT_LINE('Ambos jugadores han caido en batalla!!');
         var_mc_player_one := NULL;
         var_mc_player_two := NULL;
         END IF;
   END CASE;
END procedure_module_battle;

PROCEDURE procedure_module_getStats IS
   BEGIN
   var_mc_player_one.getStats();
   var_mc_player_two.getStats();
   procedure_module_battle;
   END procedure_module_getStats;


PROCEDURE procedure_module_init IS
   BEGIN
   IF var_mc_player_one IS NULL AND var_n_pos <= var_list_players.count THEN
      var_mc_player_one := new BATTLE_CHARACTER(
      param_id => var_n_pos, 
      param_name => var_list_players(var_n_pos)
      );
      var_mc_player_one.lvl_up();
    var_n_pos := var_n_pos+1; 
   END IF;

   IF var_mc_player_two IS NULL AND var_n_pos <= var_list_players.count THEN
      var_mc_player_two := new BATTLE_CHARACTER (
      param_id => var_n_pos, 
      param_name => var_list_players(var_n_pos)
      );
      var_mc_player_two.lvl_up();
      var_n_pos := var_n_pos+1; 
   END IF;

   IF var_mc_player_one IS NOT NULL AND var_mc_player_two IS NOT NULL THEN
      procedure_module_getStats;
   END IF;
    END procedure_module_init;   

BEGIN
   var_n_pos:= 1;
   var_list_players := SPLIT(PARAM_CHARACTERS);
   WHILE(var_n_pos <= var_list_players.count) 
   LOOP
      procedure_module_init;
         DBMS_OUTPUT.PUT_LINE(CHR(10)||CHR(10)||CHR(10));

   END LOOP;

   CASE
      WHEN var_mc_player_one IS NULL AND var_mc_player_two IS NOT NULL THEN DBMS_OUTPUT.PUT_LINE(var_mc_player_two.get_name()||' tiene la victoria definitiva!');
      WHEN var_mc_player_two IS NULL AND var_mc_player_one IS NOT NULL THEN DBMS_OUTPUT.PUT_LINE(var_mc_player_one.get_name()||' tiene la victoria definitiva!');
      ELSE DBMS_OUTPUT.PUT_LINE('Todos los jugadores han muerto!!');
      END CASE;


EXCEPTION
WHEN OTHERS THEN
   CASE
      WHEN SQLCODE = -06502  THEN DBMS_OUTPUT.PUT_LINE('Las batallas han durado demasiado y se ha hecho muy tarde. La lucha ha quedado suspendida.');
      ELSE
      dbms_output.enable(1000000);
      DBMS_OUTPUT.PUT_LINE(CHR(10)||'Las batallas han durado demasiado y se ha hecho muy tarde. La lucha ha quedado suspendida.');      END CASE;

END MODULED_BATTLE_GAME
