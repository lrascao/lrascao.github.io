---
layout: post
title: Opsworks provisioned Auto Scaling Groups
permalink: "/opsworks-provisioned-auto-scaling-groups/"
published: true
---

### TL;DR

Provision your automatically scaled EC2 instances using Opsworks

### Summary

 I've been using Cloudformation/Opsworks for some time to (almost) completely automate the launch of new infrastructure. Chef's Supermarket provides us with a lot of off-the-shelf recipes that are easy to use.

 However Opsworks misses a crucial (imo) feature which is application-based auto healing, the one that it possesses only performs health checks on the instance itself and not on an application being run. This is kinda weird since it knows the ELB status and could perform the same auto-healing that is applying when the Opsworks agent fails to ping home. As the [AWS documentation](http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-autohealing.html) tells us:

```
Every instance has an AWS OpsWorks agent that communicates
regularly with the service. AWS OpsWorks uses that
communication to monitor instance health. If an agent does
not communicate with the service for more than approximately
five minutes, AWS OpsWorks considers the instance to have failed.
```

 This is where Auto Scaling Groups comes in, it does exactly that, you can configure a ELB health check timeout, if that expires on an unhealth instance it's taken down and a new one is launched to replace it. ASG are linked to Auto Scaling Configurations which basically are shell scripts that are run when the instance is launched, so why not marry the two and have the best of both worlds?


 Just configure a normal AS configuration plus a AS group, you will use however a special script that registers the new instance with Opsworks and request for provisioning. There are a few gotchas though, the first being that since the auto-healing functionality will be implemented by ASG you *must* disable Opsworks auto-healing.


If you're on a VPC you'll run into an [Ohai issue](https://tickets.opscode.com/browse/OHAI-310) where it fails to detect that it's running on an EC2 instance, the symptoms involve your recipes failing due to not being able to extract ec2 data from the node object. We fix this by creating an empty file that helps Ohai determining that is indeed on an EC2 instance.

{% highlight shell %}
# manually add an empty hint file so that chef's ohai
# component has an easier time figuring out this is
# an ec2 instance
sudo mkdir -p /etc/chef/ohai/hints
echo > /etc/chef/ohai/hints/ec2.json
{% endhighlight %}

We'll need [jq](https://stedolan.github.io/jq/) so install it right away
{% highlight shell %}
# install jq unattended so we can parse results from the aws cli
sudo yum install jq --assumeyes
{% endhighlight %}


There are two phases for Opsworks provisioning, first we must register the instance and then assign it to a layer.
I'm using a form of [convention over configuration](https://en.wikipedia.org/wiki/Convention_over_configuration) so there are only four inputs to the script: Product, Environment, Platform and Major version, all names are derived
from these values.
The Opsworks stack name is an example of this: `v$MAJOR $ENVIRONMENT $PLATFORM`.

{% highlight shell %}
# first thing is to find the opsworks stack based on
# environment/platform
aws opsworks --region us-east-1 describe-stacks > /tmp/stacks.json
STACK_NAME=\""v$MAJOR $ENVIRONMENT $PLATFORM"\"
# use the 'jq' tool to extract the stack id we are interested in
STACK_ID=`eval "cat /tmp/stacks.json | jq '.Stacks[] | select(.Name == $STACK_NAME).StackId'"`
STACK_ID=`echo $STACK_ID | tr -d '"'`
{% endhighlight %}

{% highlight shell %}
# get intance metadata
INSTANCE_ID=`curl http://169.254.169.254/latest/meta-data/instance-id`
EC2_AVAIL_ZONE=`curl -s http://169.254.169.254/latest/meta-data/placement/availability-zone`
EC2_REGION="`echo \"$EC2_AVAIL_ZONE\" | sed -e 's:\([0-9][0-9]*\)[a-z]*\$:\\1:'`"
{% endhighlight %}

{% highlight shell %}
# determine the Opsworks layer id
aws opsworks describe-layers --region us-east-1 --stack-id $STACK_ID > /tmp/layers.json
LAYER_ID=`eval "cat /tmp/layers.json | jq '.Layers[] | select(.Shortname == \"$LAYER_SHORT_NAME\").LayerId'"`
LAYER_ID=`echo $LAYER_ID | tr -d '"'`
{% endhighlight %}


Autoscaling group instances are launched with no name, so we ask Opsworks for one and register the instance.
{% highlight shell %}
# get an Opsworks suggested name
aws opsworks get-hostname-suggestion --region us-east-1 --layer-id $LAYER_ID > /tmp/hostname-suggestion.json
HOSTNAME_SUGGESTION=`eval "cat /tmp/hostname-suggestion.json | jq '.Hostname'"`
HOSTNAME_SUGGESTION=`echo $HOSTNAME_SUGGESTION | tr -d '"'`
# and tag the instance with that name
aws ec2 create-tags --region $EC2_REGION --resources $INSTANCE_ID --tags Key=Name,Value="v$MAJOR $ENVIRONMENT $PLATFORM - $HOSTNAME_SUGGESTION"
{% endhighlight %}

{% highlight shell %}
# register the instance with Opsworks
aws opsworks register --region us-east-1 --stack-id $STACK_ID --infrastructure-class ec2 --override-hostname $HOSTNAME_SUGGESTION --local
{% endhighlight %}

Now a bit of busy waiting until the registering process is complete.

{% highlight shell %}
# get all the instances for this stack
aws opsworks describe-instances --region us-east-1 --stack-id $STACK_ID > /tmp/stack.json
# extract the opsworks instance id for this ec2 instance
OPSWORKS_INSTANCE_ID=`eval "cat /tmp/stack.json | jq '.Instances[] | select(.Ec2InstanceId == \"$INSTANCE_ID\").InstanceId'"`
OPSWORKS_INSTANCE_ID=`echo $OPSWORKS_INSTANCE_ID | tr -d '"'`
{% endhighlight %}

{% highlight shell %}
# loop until the registration is complete
OPSWORKS_INSTANCE_STATUS="unknown"
while [  $OPSWORKS_INSTANCE_STATUS != "registered" ]; do
    aws opsworks describe-instances --region us-east-1 --instance-ids $OPSWORKS_INSTANCE_ID > /tmp/instance.json
    OPSWORKS_INSTANCE_STATUS=`eval "cat /tmp/instance.json | jq '.Instances[] | select(.Ec2InstanceId == \"$INSTANCE_ID\").Status'"`
    OPSWORKS_INSTANCE_STATUS=`echo $OPSWORKS_INSTANCE_STATUS | tr -d '"'`
    echo -n "."
    # sleep a bit
    sleep 5
done
{% endhighlight %}

And finally assign the instance to the desired layer, this is the bit where the Chef recipes will be run.

{% highlight shell %}
# assign the instance to the layer
aws opsworks assign-instance --region us-east-1 --instance-id $OPSWORKS_INSTANCE_ID --layer-id $LAYER_ID
{% endhighlight %}

And that's it, if for some reason your application crashes ASG will take over and launch a new instance that will be provisioned by Opsworks.
