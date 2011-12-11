# recap some definition to make this definition self-contained
import 'org.apache.hadoop.hbase.client.HTable'
import 'org.apache.hadoop.hbase.client.Put'

def jbytes( *args )
  args.map { |arg| arg.to_s.to_java_bytes }
end

# actual exercise
def put_many( table_name, row, column_values)
  table = HTable.new( @hbase.configuration, table_name )

  p = Put.new( *jbytes( row ))

  column_values.each do |k, v|
    (kf, kn) = k.split(':')
    kn ||= ""
    p.add( *jbytes( kf, kn, v ))
  end

  table.put( p )
end
