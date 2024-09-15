package studentConsulting.service;

import java.util.List;

import studentConsulting.model.entity.notification.NotificationEntity;

public interface INotificationService {
    void sendNotification(NotificationEntity notification);
    List<NotificationEntity> getNotificationsByReceiverId(Integer receiverId);

}
