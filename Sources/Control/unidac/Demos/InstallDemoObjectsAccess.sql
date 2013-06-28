CREATE TABLE DEPT (
    DEPTNO  INTEGER NOT NULL PRIMARY KEY,
    DNAME   VARCHAR(14),
    LOC     VARCHAR(13)
);

CREATE TABLE EMP (
    EMPNO     INTEGER NOT NULL PRIMARY KEY,
    ENAME     VARCHAR(10),
    JOB       VARCHAR(9),
    MGR       INTEGER,
    HIREDATE  TIMESTAMP,
    SAL       DOUBLE,
    COMM      DOUBLE,
    DEPTNO    INTEGER REFERENCES DEPT (DEPTNO)
);

CREATE TABLE UniDAC_BLOB (
  ID INTEGER NOT NULL PRIMARY KEY,
  Title VARCHAR(30),
  Picture MEMO
);

CREATE TABLE UniDAC_TEXT (
  ID INTEGER NOT NULL PRIMARY KEY,
  Title VARCHAR(30),
  TextField MEMO
);

CREATE TABLE UniDAC_Loaded (
  Intg INTEGER,
  Dbl  DOUBLE,
  Str  VARCHAR(50),
  Dat  DATE
);

CREATE TABLE CRGRID_TEST (
  Id INTEGER NOT NULL PRIMARY KEY,
  Name VARCHAR(10),
  Country VARCHAR(30),
  City VARCHAR(30),
  Street VARCHAR(30),
  BirthDate DATE,
  Job VARCHAR(9),
  Hiredate DATE,
  Sal DOUBLE,
  Remarks MEMO
);

INSERT INTO DEPT VALUES (10,'ACCOUNTING','NEW YORK');
INSERT INTO DEPT VALUES (20,'RESEARCH','DALLAS');
INSERT INTO DEPT VALUES (30,'SALES','CHICAGO');
INSERT INTO DEPT VALUES (40,'OPERATIONS','BOSTON');

INSERT INTO EMP VALUES (7369,'SMITH','CLERK',7902,'1980-12-17 00:00:00',800,NULL,20);
INSERT INTO EMP VALUES (7499,'ALLEN','SALESMAN',7698,'1981-2-20 00:00:00',1600,300,30);
INSERT INTO EMP VALUES (7521,'WARD','SALESMAN',7698,'1981-2-22 00:00:00',1250,500,30);
INSERT INTO EMP VALUES (7566,'JONES','MANAGER',7839,'1981-4-2 00:00:00',2975,NULL,20);
INSERT INTO EMP VALUES (7654,'MARTIN','SALESMAN',7698,'1981-9-28 00:00:00',1250,1400,30);
INSERT INTO EMP VALUES (7698,'BLAKE','MANAGER',7839,'1981-5-1 00:00:00',2850,NULL,30);
INSERT INTO EMP VALUES (7782,'CLARK','MANAGER',7839,'1981-6-9 00:00:00',2450,NULL,10);
INSERT INTO EMP VALUES (7788,'SCOTT','ANALYST',7566,'1987-7-13 00:00:00',3000,NULL,20);
INSERT INTO EMP VALUES (7839,'KING','PRESIDENT',NULL,'1981-11-17 00:00:00',5000,NULL,10);
INSERT INTO EMP VALUES (7844,'TURNER','SALESMAN',7698,'1981-9-8 00:00:00',1500,0,30);
INSERT INTO EMP VALUES (7876,'ADAMS','CLERK',7788,'1987-7-13 00:00:00',1100,NULL,20);
INSERT INTO EMP VALUES (7900,'JAMES','CLERK',7698,'1981-12-3 00:00:00',950,NULL,30);
INSERT INTO EMP VALUES (7902,'FORD','ANALYST',7566,'1981-12-3 00:00:00',3000,NULL,20);
INSERT INTO EMP VALUES (7934,'MILLER','CLERK',7782,'1982-1-23 00:00:00',1300,NULL,10);

INSERT INTO CRGRID_TEST (Id, Name, Country, City, Street, BirthDate, Job, HireDate, Sal)
    VALUES (5001, 'SMITH', 'ENGLAND', 'LONDON', 'BOND st.', '1963-11-12 00:00:00', 'CLERK','1980-12-17 00:00:00', 800);
INSERT INTO CRGRID_TEST (Id, Name, Country, City, Street, BirthDate, Job, HireDate, Sal)
    VALUES (5002, 'ALLEN', 'ENGLAND', 'LONDON', 'BAKER st.', '1961-03-04 00:00:00', 'SALESMAN','1981-02-20 00:00:00', 1600);
INSERT INTO CRGRID_TEST (Id, Name, Country, City, Street, BirthDate, Job, HireDate, Sal)
    VALUES(5003, 'MARTIN', 'FRANCE', 'LION', 'WEAVER st.', '1957-01-23 00:00:00', 'MANAGER','1981-04-02 00:00:00', 2900);