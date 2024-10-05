package studentConsulting.model.entity.communication;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import studentConsulting.constant.enums.MessageStatus;
import studentConsulting.model.entity.user.UserInformationEntity;

import javax.persistence.*;
import java.time.LocalDateTime;
import java.util.Objects;

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
    private LocalDateTime date;

    @Enumerated(EnumType.STRING)
    private MessageStatus messageStatus;

    @Column(name = "recalled_for_everyone", columnDefinition = "boolean default false")
    private Boolean recalledForEveryone = false;

    @Column(name = "edited", columnDefinition = "boolean default false")
    private Boolean edited = false;

    @Column(name = "edited_date")
    private LocalDateTime editedDate;

    @Override
    public int hashCode() {
        return id != null ? id.hashCode() : 0;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        MessageEntity that = (MessageEntity) o;

        return Objects.equals(id, that.id);
    }
}
