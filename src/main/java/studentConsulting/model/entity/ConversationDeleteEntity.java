package studentConsulting.model.entity;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.*;
import java.io.Serializable;
import java.util.Objects;

@Data
@Builder
@Entity
@Table(name = "conversation_delete")
@NoArgsConstructor
@AllArgsConstructor
public class ConversationDeleteEntity implements Serializable {

    @EmbeddedId
    private ConversationDeleteKeyEntity id;

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

        ConversationDeleteEntity that = (ConversationDeleteEntity) o;

        return Objects.equals(id, that.id);
    }
}
