require 'rubygems'
require 'hpricot'
require 'open-uri'
require 'csv'

class AnalyzeNewYorkerResults 
  @@api_key = "707761c69508d89969f64264a44e96dd459f8d2c047fd0365166523625c31c6d"
  def self.api_key 
    @@api_key
  end
  
  def self.ip_to_location ip
    doc = Hpricot(open("http://api.ipinfodb.com/v2/ip_query.php?key=#{@@api_key}&ip=#{ip}&timezone=true"))
    #return {:latitude => doc.search("latitude").inner_html, :longitude => doc.search("longitude").inner_html}
    return doc.search("latitude").inner_html + "," + doc.search("longitude").inner_html
  end
  
  def self.get_locations_for_all_ip_addresses filename
    puts filename
    reader = CSV.open(filename, "r")
    header = reader.shift
    
    #puts "latitude, longitude"
    reader.each do |row|
      location = self.ip_to_location(row[33])
      puts row[33] + ", " + row[34]+", " + location#location[:latitude] + "," + location[:longitude]
    end
  end
  
  def self.get_google_maps_markers_strings filename
    puts filename
    reader = CSV.open(filename, "r")
    header = reader.shift
    
    caption1_markers = "markers=size:mid|color:green|label:1|"
    caption2_markers = "markers=size:mid|color:0x00FFFFFF|label:2|"
    caption3_markers = "markers=size:mid|color:blue|label:3|"
    none_markers = "markers=size:mid|color:purple|label:0|"
    
    reader.each do |row|
      if (row[2] != nil && row[3] != nil) then
        location = row[2].strip + "," + row[3].strip
        #puts row[1]
        case row[1]
          when " caption1"
            caption1_markers << location + "|"
          when " caption2"
            caption2_markers << location + "|"
          when " caption3"
            caption3_markers << location + "|"
          else
            none_markers << location + "|"
          end
      end
    end
    
    puts caption1_markers
    puts caption2_markers
    puts caption3_markers
    puts none_markers
  end
end