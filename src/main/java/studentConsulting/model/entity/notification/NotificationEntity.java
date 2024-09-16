package studentConsulting.model.entity.notification;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import studentConsulting.constant.enums.NotificationStatus;
import studentConsulting.constant.enums.UserType;

import javax.persistence.*;


import java.sql.Timestamp;
import java.time.LocalDate;

@Data
@Builder
@Entity
@Table(name = "notification")
@NoArgsConstructor
@AllArgsConstructor
public class NotificationEntity {
    
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(nullable = false, name = "id")
    private Integer id;

    @Column(name = "sender_id", nullable = false)
    private Integer senderId;

    @Column(name = "receiver_id", nullable = false)
    private Integer receiverId;

    @Column(name = "content", nullable = false, length = 255)
    private String content;

    @Column(name = "time", nullable = false, columnDefinition = "TIMESTAMP DEFAULT CURRENT_TIMESTAMP")
    private LocalDate time;
    
    @Enumerated(EnumType.STRING)
    private UserType userType;  

    @Enumerated(EnumType.STRING)
    private NotificationStatus status = NotificationStatus.UNREAD;
}
