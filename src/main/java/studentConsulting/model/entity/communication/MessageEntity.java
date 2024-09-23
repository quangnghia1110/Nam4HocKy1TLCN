package studentConsulting.model.entity.communication;

import java.time.LocalDate;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import studentConsulting.constant.enums.MessageStatus;
import studentConsulting.model.entity.authentication.UserInformationEntity;

@Data
@Builder
@Entity
@Table(name = "messages")
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
    private LocalDate date;

    @Enumerated(EnumType.STRING)
    private MessageStatus messageStatus;
}
