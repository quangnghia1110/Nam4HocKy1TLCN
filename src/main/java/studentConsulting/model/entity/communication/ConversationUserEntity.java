package studentConsulting.model.entity.communication;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import studentConsulting.model.entity.user.UserInformationEntity;

import javax.persistence.*;
import java.io.Serializable;
import java.util.Objects;

@Data
@Builder
@Entity
@Table(name = "conversation_user")
@NoArgsConstructor
@AllArgsConstructor
public class ConversationUserEntity implements Serializable {

    @EmbeddedId
    private ConversationUserKeyEntity id;

    @ManyToOne
    @MapsId("conversationId")
    @JoinColumn(name = "conversation_id")
    private ConversationEntity conversation;

    @ManyToOne
    @MapsId("userId")
    @JoinColumn(name = "user_id")
    private UserInformationEntity user;

    @Override
    public int hashCode() {
        return id != null ? id.hashCode() : 0;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        ConversationUserEntity that = (ConversationUserEntity) o;

        return Objects.equals(id, that.id);
    }
}
