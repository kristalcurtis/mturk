require 'rubygems'
require 'hpricot'
require 'open-uri'

class NewYorkerExperiment
  def self.get_image_url_and_caption_options_from_website contest_ID
    #doc = Hpricot(open("http://contest.newyorker.com/CaptionContest.aspx?tab=vote&affiliate=ny-caption"))
    doc = Hpricot(open("http://contest.newyorker.com/CaptionContest.aspx?id=#{contest_ID}"))
    
    #caption1 = doc.search("//span[@id='ContestVoteSubmit1_Caption1']") - doc.search("//span[@id='ContestVoteSubmit1_Caption1']/i")
    #image = doc.search("//img[@id='ContestWinners1_ContestImage']")
    #caption1 = doc.search(".//*[@id='ContestWinners1_single']/table/tbody/tr[1]/td[3]/table/tbody/tr[1]/td/p/em/text()")
    #caption2 = doc.search(".//*[@id='ContestWinners1_single']/table/tbody/tr[1]/td[3]/table/tbody/tr[2]/td/p/em/text()")
    #caption3 = doc.search(".//*[@id='ContestWinners1_single']/table/tbody/tr[1]/td[3]/table/tbody/tr[3]/td/p/em/text()")
    
    image_url = doc.at("img#ContestWinners1_ContestImage")['src']
    cartoon_info = [ image_url ]
    #puts image_url
    
    captions = (doc/"p.cap/em")
    #captions.each do |caption|
    #  puts caption.inner_html
    #end
    #cartoon_info << captions
    captions.each do |caption|
      #caption_text = caption.inner_html.to_s.gsub(",", "").chop
      #caption_text.slice!(0)
      #cartoon_info << caption_text
      cartoon_info << caption.inner_html.to_s.gsub("\342\200\234", '"').gsub("\342\200\235", '"').gsub(",", "")
      #cartoon_info << caption.inner_html.to_s.gsub(/\342\200\23[45]/, '"')#.gsub(",", "")
    end
    
    return cartoon_info
  end
  
  def self.prep_csv_of_all_cartoon_contests max_contest=262
    # get heading
    csv = ["image_url,caption1_text,caption2_text,caption3_text"]
    
    1.upto max_contest do |i|
      csv << NewYorkerExperiment.get_image_url_and_caption_options_from_website(i).inject do |sum, n| 
        sum + "," + n
      end
    end
    
    return csv
  end
end