package studentConsulting.service.interfaces.common;

import studentConsulting.constant.enums.NotificationType;
import studentConsulting.model.entity.notification.NotificationEntity;

import java.util.List;

public interface INotificationService {
    public void sendUserNotification(Integer senderId, Integer receiverId, String content, NotificationType type);

    List<NotificationEntity> getNotificationsByReceiverId(Integer receiverId);

//    Page<studentConsulting.model.payload.dto.notification.NotificationDTO> findNotificationsByUserWithFilters(Integer userId, String content, LocalDate startDate, LocalDate endDate, Pageable pageable);

}
