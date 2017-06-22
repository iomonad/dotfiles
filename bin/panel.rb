#!/usr/bin/env ruby
# -*- coding: utf-8 -*-

require 'dzen'
include DZEN::Helpers

# Colors
COLORS = Hash.new
%x[xrdb -query | grep "*color"].split("\n").each { |c|
  colordef = c.split
  COLORS[colordef[0].gsub(/[^0-9]/, "")] = colordef[1]
}

# Icons
ICONPATH = "#{ENV["HOME"]}/.config/stumpwm/dzen/icons"

def icon(file)
  return "^i(%s/%s.xbm) " % [ ICONPATH, file ]
end

# WiFi
INTERFACE = "wlp3s0"

@rxo = IO.read("/sys/class/net/#{INTERFACE}/statistics/rx_bytes").to_i
@txo = IO.read("/sys/class/net/#{INTERFACE}/statistics/tx_bytes").to_i

# IRC
WEECHATLOG = "#{ENV["HOME"]}/.weechat/logs/perl.highmon.weechatlog"
IRSSILOG = "#{ENV["HOME"]}/.irssi/logs/hilights.log"

# Mail
MAILBOXES = %w{Gmail Mail}

configure do |c|
  c.interval = 1
  c.delimiter = "  "
  c.output = $stdout
end

before_run do
  "--- loading ---#{DZEN::Base::Config[:ending]}#{updatepacman}"
end

app :cpu do
  "^pa(685)" + _color(COLORS["1"], icon("temp")) +
  %x[sensors].scan(/Core \d:\s+\+(\d+)\.\d/).flatten.map{|e|"#{e}°C"}.join(" ")
end

app :loadavg do
  _color(COLORS["3"], icon("cpu")) +
  IO.read('/proc/loadavg').split(/ /)[0,3].join(' ')
end

app :speed do
  _color(COLORS["5"], icon("wifi")) + "#{speed}"
end

def speed
  rx = IO.read("/sys/class/net/#{INTERFACE}/statistics/rx_bytes").to_i
  tx = IO.read("/sys/class/net/#{INTERFACE}/statistics/tx_bytes").to_i
  string = "%3ik %2ik" % [((rx - @rxo) / 1024), ((tx - @txo) / 1024)]
  @rxo, @txo = rx, tx
  return string
end


app :time do
  _color(COLORS["2"], icon("clock1")) + Time.now.strftime("%d.%m.%Y %H:%M")
end

app :weechat do
  def weechat
    msg = IO.readlines(WEECHATLOG)[-1].split(/ /)[1].split("\t")
    return [msg[0], msg[1].split("#")[1], msg[2]].join(" ")
  end

  _color(COLORS["4"], icon("balloon")) + weechat
end

app :irssi do
  def irssi
    msg = IO.readlines(IRSSILOG)[-1].gsub(/[\s]+/, "\s").split(/ /)
    return "%s %s <%s>" % [msg[0], msg[1], msg[2]]
  end

  _color(COLORS["4"], icon("balloon")) + irssi
end

def updatepacman
  Thread.new {
    loop do
      $updates = %x[clyde -Qu].split("\n")
      sleep 300
    end
  }
end

app :pacman do

  def pacman
    Thread.new {
      sleep 1
      @string = _color(COLORS["6"], icon("pacman"))
      ["core/", "extra/", "community/"].each { |r|
        @string += " " + $updates.find_all { |a|
          a.include? r
        }.size.to_s
      }
    }
    return @string
  end

  pacman
end

app :groups do
  "^pa(10)" + getgroups
end

def getgroups
  groups, active, string, id = Array.new, Array.new, "", 0

  %x[wmctrl -d].split("\n").each { |g|
    groups.concat([g.split.last])
    active.concat([g.split[1]])
  }
  hash = Hash[groups.zip(active)]

  hash.each { |g,a|
    string += "^ca(1,wmctrl -s #{id})"
    if a == "*"
      string += "^bg(#{COLORS["12"]}) #{g.upcase} ^bg()"
    else
      string += " #{g.upcase} "
    end
    string += "^ca()"
    id += 1
  }
  return string
end

app :mail do
  _color(COLORS["2"], icon("mail")) + mail
end

def mail
  count = ""
  MAILBOXES.each { |m|
    count += " " + (Dir.entries("/media/data/Mail/#{m}/INBOX/new").size - 2).to_s
  }
  return count
end

order :groups, :cpu, :loadavg, :speed, :time, :irssi, :pacman, :mail
