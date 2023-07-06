load('./cpo_joint_log/mixed_joint/log_cpo_mixed_joint.RData')
load('./cpo_joint_log/ar_joint/log_cpo_ar_joint.RData')
load('./cpo_joint_log/ar_me_joint/log_cpo_arme_joint2.RData')

format(log_cpo_mixed_joint, scientific = F)
format(log_cpo_ar_joint, scientific = F)
format(log_cpo_arme_joint, scientific = F)


# log_cpo_arme_jointw <- log_cpo_arme_joint
# 
# mean(exp(log_cpo_arme_jointw))
# mean(exp(log_cpo_arme_joint))
