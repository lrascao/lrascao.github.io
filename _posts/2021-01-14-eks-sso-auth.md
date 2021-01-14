---
layout: post
title: EKS SSO authentication
permalink: "/eks-sso-auth/"
published: true
---

This is a short post on how to configure EKS authentication with AWS Federated Logins (Single Sign On).

* [awscli SSO](#awscli-sso)
* [awsvault](#awsvault)
* [EKS credentials](#eks-credentials)
* [EKS aws-auth](#eks-aws-auth)

### [awscli SSO](#awscli-sso)

After having your SSO configured and being able to log in to the AWS console you'll also want to be able to use the awscli, for that you'll need an entry similar to this one in your `.aws/config` file:

{% highlight config %}

[profile sso-profile]
sso_start_url = https://domain.awsapps.com/start
sso_region = us-east-1
sso_account_id = account-id
sso_role_name = role-name
region = us-west-2
{% endhighlight %}

The parameters you'll want to change are:
* `sso-profile`: The name of your awscli profile
* `domain`: You'll need to get this from whomever configured SSO
* `account-id`: Your AWS account id
* `role-name`: The role name containing the allowed policies

Setting up is also straightforward:

{% highlight shell %}
$ aws sso login --profile sso-profile
{% endhighlight %}

You can now look up the `role-name` in your AWS IAM console, its ARN you'll look something like this:

`arn:aws:iam::account-id:role/aws-reserved/sso.amazonaws.com/AWSReservedSSO_role-name_id`

Notice the `aws-reserved/sso.amazonaws.com/` bit, this will be relevant further ahead.

### [awsvault](#awsvault)

[awsvault](https://github.com/99designs/aws-vault) is a great tool to help you manage your AWS credentials/profiles, it's easy to get it up and running. Once installed it will consult your `.aws/config` and manage the credentials
of those profiles for you, the `list` command gives you the list of profiles and associated credentials:

{% highlight shell %}
$ aws-vault list
{% endhighlight %}

Adding credentials for the SSO profile is a matter of:

{% highlight shell %}
$ aws-vault login sso-profile
{% endhighlight %}

This should open up your browser and perform the authentication, after that you're able to use it with any awscli commands:

{% highlight shell %}
$ aws-vault exec sso-profile -- aws sts get-caller-identity
{
    "UserId": user-id,
    "Account": account-id,
    "Arn": "arn:aws:sts::account-id:assumed-role/AWSReservedSSO_role-id_id/user-id"
}
{% endhighlight %}

On MacOS, `aws-vault` stores it's secrets in `~/Library/Keychains/aws-vault.keychain-db`, you'll probably want to open that file with `Keychain Access` tool should you ever need to delete things manually.

### [EKS credentials](#eks-credentials)

Now that we have our `aws-vault` working we can apply it to Kubernetes authentication, let's give it a spin:

{% highlight shell %}
$ aws-vault exec sso-profile -- aws-iam-authenticator token --cluster-id eks-cluster-name --cache
{"kind": "ExecCredential", "apiVersion": "client.authentication.k8s.io/v1alpha1", "spec": {}, "status": {"expirationTimestamp": "2021-01-14T10:42:37Z", "token": bearer-token}}
{% endhighlight %}

Notice the JSON reply, it is an [`ExecCredential`](https://kubernetes.io/docs/reference/access-authn-authz/authentication/#input-and-output-formats) k8s resource, this is the expected output format of whatever you add to your `~/.kube/config` file.
[`aws-iam-authenticator`](https://github.com/kubernetes-sigs/aws-iam-authenticator) is an app maintained by Kubernetes that takes care of this for us.

Below is an entry of `~/.kube/config`, `users` section with the relevant changes:
{% highlight yaml %}
- name: user-entry-name
  user:
    exec:
      apiVersion: client.authentication.k8s.io/v1alpha1
      command: aws-vault
      args:
      - exec
      - sso-profile
      - --
      - aws-iam-authenticator
      - token
      - --cluster-id
      - eks-cluster-name
      - --cache
{% endhighlight %}

### [EKS aws-auth](#eks-aws-auth)

Final thing left to do is informing the EKS cluster that this role is allowed to perform operations in the cluster, this is the tricky bit, to do that we'll need to edit a ConfigMap in the cluster. This means you'll need to already have access to the cluster, hopefully whoever created the cluster
is around to help you in this part:

{% highlight shell %}
$ kubectl edit configmap aws-auth —namespace kube-system
{% endhighlight %}

The relevant bit to add is beneath `data`, `mapRoles`.
Important thing to notice is the `roleArn` field and the fact that it doesn't contain the `aws-reserved/sso.amazonaws.com/` i mentioned at the beginning of the post. You'll need to remove that otherwise you won't be able to authenticate successfully (no idea why is that).

{% highlight yaml %}
apiVersion: v1
data:
  mapRoles: |
    - rolearn: arn:aws:iam::account-id:role/AWSReservedSSO_role-name_id
      username: some-k8s-username
      groups:
      - system:masters
{% endhighlight %}

And that's it, on MacOS now and then `aws-vault` will request you to unlock the Keychain but that's about it.

<!-- 
Notes
-->

