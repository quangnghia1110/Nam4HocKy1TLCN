package studentConsulting.model.entity.communication;

import javax.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import studentConsulting.model.entity.authentication.UserInformationEntity;

import java.sql.Timestamp;
import java.util.Set;

@Data
@Builder
@Entity
@Table(name = "messages")
@NoArgsConstructor
@AllArgsConstructor
public class MessageEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id", nullable = false)
    private Integer id; // Mã tin nhắn

    @ManyToOne
    @JoinColumn(name = "conversation_id", nullable = false, referencedColumnName = "id")
    private ConversationEntity conversation; // Mã phòng nhắn tin

    @ManyToOne
    @JoinColumn(name = "user_id", nullable = false, referencedColumnName = "id")
    private UserInformationEntity sender; // Mã người gửi tham chiếu

    @Column(name = "message_text", nullable = false, length = 255)
    private String messageText; // Đoạn tin nhắn

    @Column(name = "sent_at", nullable = false)
    private Timestamp sentAt; // Gửi vào lúc

    @Column(name = "type", nullable = false, length = 255)
    private String type; // văn bản, hình ảnh

    @Column(name = "status_read", nullable = false)
    private Boolean statusRead; // Đã đọc, chưa đọc

    @Column(name = "status_send", nullable = false)
    private Boolean statusSend; // Gửi thành công, thất bại

    @Column(name = "status_recall", nullable = false)
    private Boolean statusRecall; // Thu hồi, không thu hồi

    @OneToMany(mappedBy = "message", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<MessageAttachmentEntity> attachments; // Danh sách đính kèm tin nhắn

    @OneToMany(mappedBy = "message", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<MessageImageEntity> images; // Danh sách hình ảnh tin nhắn
}
