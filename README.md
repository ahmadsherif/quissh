# quissh
**Qui**ckly **SSH** to your EC2 machines.

#Usage
I'm assuming Erlang/OTP 17 or greater is installed. I'm not an Ubuntu user, but I think `erlang-dev` package is required too.

AWS credentials provided need to have `ec2:DescribeInstances` permission allowed.

```
git clone git@github.com:ahmadsherif/quissh.git
cd quishh
make app

AWS_ACCESS_KEY_ID=xxx AWS_SECRET_KET=xxx AWS_REGION=xxx ./quissh
```

#FAQ
##Why quissh?
I SSH to different machines at work a lot. When the number of the machines were small (one or two)
you can use your shell histroy to quickly access them. But when the number increases and old machines
get torn down and new ones are launched, shell history is useless. I used to open AWS dashboard to get
the IP of the machine I wanted to access, but I thought I would do something slightly better.

##Why Erlang?
I think it's very interesing and a lot of fun. Not particularly a good choice for command-line
application (naturally I would use Ruby or Go), but hey, pattern matching is awesome.

##Is it limited to EC2 only?
Currently yes. But I think extending it to other providers shouldn't be a painful task.
