#!/usr/bin/ruby1.9
# requires ruby 1.9
require "net/http"

# usage:
# ./getstreetviewmaps input-file output-dir
#	input is the file with the lat/longs to process. 
#	format: must contain only header lines starting with a pound symbol, like:
#		# 12345 optional comment here
#	or comma-separated lat,long
#		33.123,-122.44
#	output is the directory to store the images in. 
# will create a subdirectory for each zip code. 

store_in = ARGV[1]
f = File.new ARGV[0]

if File.directory? store_in
	raise "directory #{store_in} already exists, please select another"
end
Dir.mkdir store_in
Dir.chdir store_in

zipheader = /^# ?(\d{5})/

# save each file in a directory named after its zip code
subdir = false 
f.readlines.each do |line|
	if (zip = line[zipheader,1])
		if subdir
			Dir.chdir "../"
		end
		Dir.mkdir zip 
		Dir.chdir zip
		subdir = true
		puts "processing zip #{zip}"
	elsif line.strip.length > 0
		lat, lng = line.split(/, ?/)
		lat = lat.strip
		lng = lng.strip
		# format streetview api call and retrieve one image for each field of view. 
		[50,90,120].each do |fov|
			url = "http://maps.googleapis.com/maps/api/streetview?size=600x300&location=#{lat},#{lng}&fov=#{fov}&pitch=5&sensor=false"
			puts "retreiving #{url}..."
			# download image
			uri = URI("http://maps.googleapis.com/maps/api/streetview")
			params = { :size => "600x300", :location => "#{lat},#{lng}", :fov =>fov, :pitch => 5, :sensor => false}
			uri.query = URI.encode_www_form(params)
			res = Net::HTTP.get_response(uri)
			img_file = "streetview-600x300-#{lat},#{lng}-fov#{fov}-pitch5.jpg"
			img = open(img_file, "wb")
			# TODO check if image exists or not before writing it - if
			# possible? API docs dn specify how to detect. 
			img.write(res.body) if res.is_a?(Net::HTTPSuccess)
			img.close
		end
	end
end

