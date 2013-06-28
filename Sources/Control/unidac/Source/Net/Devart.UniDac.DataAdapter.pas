unit Devart.UniDac.DataAdapter;

interface

uses 
  DB, System.Data.Common, System.Data, Devart.Dac.DataAdapter,
  Variants, System.ComponentModel;

type
  TDataTableArr = array of DataTable;
  IDataParameterArr = array of IDataParameter;

  UniDataAdapter = class (DADataAdapter)
  published
    property DataSet;
  end;

implementation

end.
