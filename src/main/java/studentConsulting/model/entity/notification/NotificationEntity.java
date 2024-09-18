package studentConsulting.model.entity.notification;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import studentConsulting.constant.enums.NotificationStatus;
import studentConsulting.constant.enums.NotificationType;

import javax.persistence.*;


import java.sql.Timestamp;
import java.time.LocalDate;
import java.time.LocalDateTime;

@Data
@Builder
@Entity
@Table(name = "notification")
@NoArgsConstructor
@AllArgsConstructor
public class NotificationEntity {
    
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column( name = "id")
    private Integer id;

    @Column(name = "sender_id")
    private Integer senderId;

    @Column(name = "receiver_id")
    private Integer receiverId;

    @Column(name = "content",  length = 255)
    private String content;

    @Column(name = "time")
    private LocalDateTime time;
    
    @Enumerated(EnumType.STRING)
    private NotificationType notificationType;  

    @Enumerated(EnumType.STRING)
    private NotificationStatus status = NotificationStatus.UNREAD;
}
