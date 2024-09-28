package studentConsulting.model.entity.communication;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import studentConsulting.constant.enums.MessageStatus;
import studentConsulting.model.entity.user.UserInformationEntity;

import javax.persistence.*;
import java.time.LocalDate;

@Data
@Builder
@Entity
@Table(name = "message")
@NoArgsConstructor
@AllArgsConstructor
public class MessageEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Integer id;

    @Column(name = "conversation_id")
    private Integer conversationId;

    @ManyToOne
    @JoinColumn(name = "sender_id", nullable = false)
    private UserInformationEntity sender;

    @ManyToOne
    @JoinColumn(name = "receiver_id", nullable = false)
    private UserInformationEntity receiver;

    private String message;
    private String imageUrl;
    private LocalDate date;

    @Enumerated(EnumType.STRING)
    private MessageStatus messageStatus;
}
