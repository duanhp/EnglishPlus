object DM: TDM
  OldCreateOrder = False
  Left = 188
  Top = 108
  Height = 279
  Width = 396
  object Connection: TUniConnection
    Options.DisconnectedMode = True
    Options.LocalFailover = True
    Pooling = True
    Left = 16
    Top = 16
  end
  object quDetail: TUniQuery
    Connection = Connection
    SQL.Strings = (
      'select * from emp')
    MasterSource = dsMaster
    MasterFields = 'DEPTNO'
    DetailFields = 'DEPTNO'
    Debug = True
    CachedUpdates = True
    Options.LocalMasterDetail = True
    Left = 88
    Top = 72
  end
  object quMaster: TUniQuery
    Connection = Connection
    SQL.Strings = (
      'select * from dept'
      '')
    Debug = True
    CachedUpdates = True
    Left = 88
    Top = 16
  end
  object dsDetail: TDataSource
    DataSet = quDetail
    Left = 120
    Top = 72
  end
  object dsMaster: TDataSource
    DataSet = quMaster
    Left = 120
    Top = 16
  end
  object scCreate_MySQL: TUniScript
    SQL.Strings = (
      'CREATE TABLE DEPT ('
      '  DEPTNO INT PRIMARY KEY,'
      '  DNAME VARCHAR(14),'
      '  LOC VARCHAR(13)'
      ');'
      'INSERT INTO DEPT VALUES (10,'#39'ACCOUNTING'#39','#39'NEW YORK'#39');'
      'INSERT INTO DEPT VALUES (20,'#39'RESEARCH'#39','#39'DALLAS'#39');'
      'INSERT INTO DEPT VALUES (30,'#39'SALES'#39','#39'CHICAGO'#39');'
      'INSERT INTO DEPT VALUES (40,'#39'OPERATIONS'#39','#39'BOSTON'#39');'
      ''
      'CREATE TABLE EMP ('
      '  EMPNO INT PRIMARY KEY,'
      '  ENAME VARCHAR(10),'
      '  JOB VARCHAR(9),'
      '  MGR INT,'
      '  HIREDATE DATETIME,'
      '  SAL FLOAT,'
      '  COMM FLOAT,'
      '  DEPTNO INT REFERENCES DEPT'
      ');'
      
        'INSERT INTO EMP VALUES (7369,'#39'SMITH'#39','#39'CLERK'#39',7902,'#39'1980-12-17'#39',8' +
        '00,NULL,20);'
      
        'INSERT INTO EMP VALUES (7499,'#39'ALLEN'#39','#39'SALESMAN'#39',7698,'#39'1981-2-20'#39 +
        ',1600,300,30);'
      
        'INSERT INTO EMP VALUES (7521,'#39'WARD'#39','#39'SALESMAN'#39',7698,'#39'1981-2-22'#39',' +
        '1250,500,30);'
      
        'INSERT INTO EMP VALUES (7566,'#39'JONES'#39','#39'MANAGER'#39',7839,'#39'1981-4-2'#39',2' +
        '975,NULL,20);'
      
        'INSERT INTO EMP VALUES (7654,'#39'MARTIN'#39','#39'SALESMAN'#39',7698,'#39'1981-9-28' +
        #39',1250,1400,30);'
      
        'INSERT INTO EMP VALUES (7698,'#39'BLAKE'#39','#39'MANAGER'#39',7839,'#39'1981-5-1'#39',2' +
        '850,NULL,30);'
      
        'INSERT INTO EMP VALUES (7782,'#39'CLARK'#39','#39'MANAGER'#39',7839,'#39'1981-6-9'#39',2' +
        '450,NULL,10);'
      
        'INSERT INTO EMP VALUES (7788,'#39'SCOTT'#39','#39'ANALYST'#39',7566,'#39'1987-7-13'#39',' +
        '3000,NULL,20);'
      
        'INSERT INTO EMP VALUES (7839,'#39'KING'#39','#39'PRESIDENT'#39',NULL,'#39'1981-11-17' +
        #39',5000,NULL,10);'
      
        'INSERT INTO EMP VALUES (7844,'#39'TURNER'#39','#39'SALESMAN'#39',7698,'#39'1981-9-8'#39 +
        ',1500,0,30);'
      
        'INSERT INTO EMP VALUES (7876,'#39'ADAMS'#39','#39'CLERK'#39',7788,'#39'1987-7-13'#39',11' +
        '00,NULL,20);'
      
        'INSERT INTO EMP VALUES (7900,'#39'JAMES'#39','#39'CLERK'#39',7698,'#39'1981-12-3'#39',95' +
        '0,NULL,30);'
      
        'INSERT INTO EMP VALUES (7902,'#39'FORD'#39','#39'ANALYST'#39',7566,'#39'1981-12-3'#39',3' +
        '000,NULL,20);'
      
        'INSERT INTO EMP VALUES (7934,'#39'MILLER'#39','#39'CLERK'#39',7782,'#39'1982-1-23'#39',1' +
        '300,NULL,10);'
      '')
    Connection = Connection
    Left = 181
    Top = 16
  end
  object scCreate_SQLServer: TUniScript
    SQL.Strings = (
      'CREATE TABLE DEPT ('
      '  DEPTNO INT PRIMARY KEY ,'
      '  DNAME VARCHAR(14) ,'
      '  LOC VARCHAR(13)'
      ');'
      ''
      'CREATE TABLE EMP ('
      '  EMPNO INT IDENTITY PRIMARY KEY,'
      '  ENAME VARCHAR(10),'
      '  JOB VARCHAR(9),'
      '  MGR INT,'
      '  HIREDATE DATETIME,'
      '  SAL FLOAT,'
      '  COMM FLOAT,'
      '  DEPTNO INT CONSTRAINT FK_DEPTNO REFERENCES DEPT'
      ');'
      ''
      'INSERT INTO DEPT VALUES (10,'#39'ACCOUNTING'#39','#39'NEW YORK'#39');'
      'INSERT INTO DEPT VALUES (20,'#39'RESEARCH'#39','#39'DALLAS'#39');'
      'INSERT INTO DEPT VALUES (30,'#39'SALES'#39','#39'CHICAGO'#39');'
      'INSERT INTO DEPT VALUES (40,'#39'OPERATIONS'#39','#39'BOSTON'#39');'
      ''
      'INSERT INTO EMP VALUES'
      '('#39'SMITH'#39','#39'CLERK'#39',7902,CAST('#39'12/17/80'#39' AS DATETIME),800,NULL,20);'
      'INSERT INTO EMP VALUES'
      '('#39'ALLEN'#39','#39'SALESMAN'#39',7698,'#39'02/20/81'#39',1600,300,30);'
      'INSERT INTO EMP VALUES'
      '('#39'WARD'#39','#39'SALESMAN'#39',7698,'#39'02/22/81'#39',1250,500,30);'
      'INSERT INTO EMP VALUES'
      '('#39'JONES'#39','#39'MANAGER'#39',7839,'#39'04/02/81'#39',2975,NULL,20);'
      'INSERT INTO EMP VALUES'
      '('#39'MARTIN'#39','#39'SALESMAN'#39',7698,'#39'09/28/81'#39',1250,1400,30);'
      'INSERT INTO EMP VALUES'
      '('#39'BLAKE'#39','#39'MANAGER'#39',7839,'#39'05/01/81'#39',2850,NULL,30);'
      'INSERT INTO EMP VALUES'
      '('#39'CLARK'#39','#39'MANAGER'#39',7839,'#39'06/09/81'#39',2450,NULL,10);'
      'INSERT INTO EMP VALUES'
      '('#39'SCOTT'#39','#39'ANALYST'#39',7566,'#39'07/13/87'#39',3000,NULL,20);'
      'INSERT INTO EMP VALUES'
      '('#39'KING'#39','#39'PRESIDENT'#39',NULL,'#39'11/17/81'#39',5000,NULL,10);'
      'INSERT INTO EMP VALUES'
      '('#39'TURNER'#39','#39'SALESMAN'#39',7698,'#39'09/08/81'#39',1500,0,30);'
      'INSERT INTO EMP VALUES'
      '('#39'ADAMS'#39','#39'CLERK'#39',7788,'#39'07/13/87'#39',1100,NULL,20);'
      'INSERT INTO EMP VALUES'
      '('#39'JAMES'#39','#39'CLERK'#39',7698,'#39'12/03/81'#39',950,NULL,30);'
      'INSERT INTO EMP VALUES'
      '('#39'FORD'#39','#39'ANALYST'#39',7566,'#39'12/03/81'#39',3000,NULL,20);'
      'INSERT INTO EMP VALUES'
      '('#39'MILLER'#39','#39'CLERK'#39',7782,'#39'01/23/82'#39',1300,NULL,10);')
    Connection = Connection
    Left = 224
    Top = 16
  end
  object scCreate_InterBase: TUniScript
    SQL.Strings = (
      'CREATE TABLE DEPT ('
      '    DEPTNO  INTEGER NOT NULL PRIMARY KEY,'
      '    DNAME   VARCHAR(14),'
      '    LOC     VARCHAR(13)'
      ');'
      ''
      'COMMIT;'
      ''
      'CREATE TABLE EMP ('
      '    EMPNO     INTEGER NOT NULL PRIMARY KEY,'
      '    ENAME     VARCHAR(10),'
      '    JOB       VARCHAR(9),'
      '    MGR       INTEGER,'
      '    HIREDATE  TIMESTAMP,'
      '    SAL       INTEGER,'
      '    COMM      INTEGER,'
      '    DEPTNO    INTEGER REFERENCES DEPT (DEPTNO)'
      ');'
      ''
      'INSERT INTO DEPT VALUES'
      '  (10,'#39'ACCOUNTING'#39','#39'NEW YORK'#39');'
      'INSERT INTO DEPT VALUES'
      '  (20,'#39'RESEARCH'#39','#39'DALLAS'#39');'
      'INSERT INTO DEPT VALUES'
      '  (30,'#39'SALES'#39','#39'CHICAGO'#39');'
      'INSERT INTO DEPT VALUES'
      '  (40,'#39'OPERATIONS'#39','#39'BOSTON'#39');'
      'INSERT INTO EMP VALUES'
      '  (7369,'#39'SMITH'#39','#39'CLERK'#39',7902,'#39'17.12.1980'#39',800,NULL,20);'
      'INSERT INTO EMP VALUES'
      '  (7499,'#39'ALLEN'#39','#39'SALESMAN'#39',7698,'#39'20.02.1981'#39',1600,300,30);'
      'INSERT INTO EMP VALUES'
      '  (7521,'#39'WARD'#39','#39'SALESMAN'#39',7698,'#39'22.02.1981'#39',1250,500,30);'
      'INSERT INTO EMP VALUES'
      '  (7566,'#39'JONES'#39','#39'MANAGER'#39',7839,'#39'02.04.1981'#39',2975,NULL,20);'
      'INSERT INTO EMP VALUES'
      '  (7654,'#39'MARTIN'#39','#39'SALESMAN'#39',7698,'#39'28.09.1981'#39',1250,1400,30);'
      'INSERT INTO EMP VALUES'
      '  (7698,'#39'BLAKE'#39','#39'MANAGER'#39',7839,'#39'01.05.1981'#39',2850,NULL,30);'
      'INSERT INTO EMP VALUES'
      '  (7782,'#39'CLARK'#39','#39'MANAGER'#39',7839,'#39'09.06.1981'#39',2450,NULL,10);'
      'INSERT INTO EMP VALUES'
      '  (7788,'#39'SCOTT'#39','#39'ANALYST'#39',7566,'#39'13.07.87'#39',3000,NULL,20);'
      'INSERT INTO EMP VALUES'
      '  (7839,'#39'KING'#39','#39'PRESIDENT'#39',NULL,'#39'17.11.1981'#39',5000,NULL,10);'
      'INSERT INTO EMP VALUES'
      '  (7844,'#39'TURNER'#39','#39'SALESMAN'#39',7698,'#39'08.09.1981'#39',1500,0,30);'
      'INSERT INTO EMP VALUES'
      '  (7876,'#39'ADAMS'#39','#39'CLERK'#39',7788,'#39'13.07.87'#39',1100,NULL,20);'
      'INSERT INTO EMP VALUES'
      '  (7900,'#39'JAMES'#39','#39'CLERK'#39',7698,'#39'03.12.1981'#39',950,NULL,30);'
      'INSERT INTO EMP VALUES'
      '  (7902,'#39'FORD'#39','#39'ANALYST'#39',7566,'#39'03.12.1981'#39',3000,NULL,20);'
      'INSERT INTO EMP VALUES'
      '  (7934,'#39'MILLER'#39','#39'CLERK'#39',7782,'#39'23.01.1982'#39',1300,NULL,10);'
      '  '
      'COMMIT;')
    Connection = Connection
    Left = 320
    Top = 16
  end
  object scCreate_Oracle: TUniScript
    SQL.Strings = (
      'CREATE TABLE DEPT ('
      '  DEPTNO NUMBER(2) CONSTRAINT PK_DEPT PRIMARY KEY,'
      '  DNAME VARCHAR2(14) ,'
      '  LOC VARCHAR2(13)'
      ');'
      ''
      'CREATE TABLE EMP ('
      '  EMPNO NUMBER(4) CONSTRAINT PK_EMP PRIMARY KEY,'
      '  ENAME VARCHAR2(10),'
      '  JOB VARCHAR2(9),'
      '  MGR NUMBER(4),'
      '  HIREDATE DATE,'
      '  SAL NUMBER(7,2),'
      '  COMM NUMBER(7,2),'
      '  DEPTNO NUMBER(2) CONSTRAINT FK_DEPTNO REFERENCES DEPT'
      ');'
      ''
      'INSERT INTO DEPT VALUES'
      '  (10,'#39'ACCOUNTING'#39','#39'NEW YORK'#39');'
      'INSERT INTO DEPT VALUES'
      '  (20,'#39'RESEARCH'#39','#39'DALLAS'#39');'
      'INSERT INTO DEPT VALUES'
      '  (30,'#39'SALES'#39','#39'CHICAGO'#39');'
      'INSERT INTO DEPT VALUES'
      '  (40,'#39'OPERATIONS'#39','#39'BOSTON'#39');'
      ''
      'INSERT INTO EMP VALUES'
      
        '  (7369,'#39'SMITH'#39','#39'CLERK'#39',7902,to_date('#39'17-12-1980'#39','#39'dd-mm-yyyy'#39'),' +
        '800,NULL,20);'
      'INSERT INTO EMP VALUES'
      
        '  (7499,'#39'ALLEN'#39','#39'SALESMAN'#39',7698,to_date('#39'20-02-1981'#39','#39'dd-mm-yyyy' +
        #39'),1600,300,30);'
      'INSERT INTO EMP VALUES'
      
        '  (7521,'#39'WARD'#39','#39'SALESMAN'#39',7698,to_date('#39'22-02-1981'#39','#39'dd-mm-yyyy'#39 +
        '),1250,500,30);'
      'INSERT INTO EMP VALUES'
      
        '  (7566,'#39'JONES'#39','#39'MANAGER'#39',7839,to_date('#39'02-04-1981'#39','#39'dd-mm-yyyy'#39 +
        '),2975,NULL,20);'
      'INSERT INTO EMP VALUES'
      
        '  (7654,'#39'MARTIN'#39','#39'SALESMAN'#39',7698,to_date('#39'28-09-1981'#39','#39'dd-mm-yyy' +
        'y'#39'),1250,1400,30);'
      'INSERT INTO EMP VALUES'
      
        '  (7698,'#39'BLAKE'#39','#39'MANAGER'#39',7839,to_date('#39'01-05-1981'#39','#39'dd-mm-yyyy'#39 +
        '),2850,NULL,30);'
      'INSERT INTO EMP VALUES'
      
        '  (7782,'#39'CLARK'#39','#39'MANAGER'#39',7839,to_date('#39'09-06-1981'#39','#39'dd-mm-yyyy'#39 +
        '),2450,NULL,10);'
      'INSERT INTO EMP VALUES'
      
        '  (7788,'#39'SCOTT'#39','#39'ANALYST'#39',7566,to_date('#39'13-07-87'#39','#39'dd-mm-yyyy'#39'),' +
        '3000,NULL,20);'
      'INSERT INTO EMP VALUES'
      
        '  (7839,'#39'KING'#39','#39'PRESIDENT'#39',NULL,to_date('#39'17-11-1981'#39','#39'dd-mm-yyyy' +
        #39'),5000,NULL,10);'
      'INSERT INTO EMP VALUES'
      
        '  (7844,'#39'TURNER'#39','#39'SALESMAN'#39',7698,to_date('#39'08-09-1981'#39','#39'dd-mm-yyy' +
        'y'#39'),1500,0,30);'
      'INSERT INTO EMP VALUES'
      
        '  (7876,'#39'ADAMS'#39','#39'CLERK'#39',7788,to_date('#39'13-07-87'#39','#39'dd-mm-yyyy'#39'),11' +
        '00,NULL,20);'
      'INSERT INTO EMP VALUES'
      
        '  (7900,'#39'JAMES'#39','#39'CLERK'#39',7698,to_date('#39'03-12-1981'#39','#39'dd-mm-yyyy'#39'),' +
        '950,NULL,30);'
      'INSERT INTO EMP VALUES'
      
        '  (7902,'#39'FORD'#39','#39'ANALYST'#39',7566,to_date('#39'03-12-1981'#39','#39'dd-mm-yyyy'#39')' +
        ',3000,NULL,20);'
      'INSERT INTO EMP VALUES'
      
        '  (7934,'#39'MILLER'#39','#39'CLERK'#39',7782,to_date('#39'23-01-1982'#39','#39'dd-mm-yyyy'#39')' +
        ',1300,NULL,10);'
      ''
      'COMMIT;')
    Connection = Connection
    Left = 272
    Top = 16
  end
  object scDrop: TUniScript
    SQL.Strings = (
      'DROP TABLE EMP;'
      'DROP TABLE DEPT;')
    Connection = Connection
    Left = 176
    Top = 72
  end
  object scDrop_InterBase: TUniScript
    SQL.Strings = (
      'DROP TABLE EMP;'
      ''
      'COMMIT;'
      ''
      'DROP TABLE DEPT;')
    Connection = Connection
    Left = 224
    Top = 72
  end
end
