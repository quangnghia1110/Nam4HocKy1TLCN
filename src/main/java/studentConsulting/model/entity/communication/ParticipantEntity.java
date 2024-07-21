package studentConsulting.model.entity.communication;

import javax.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import studentConsulting.model.entity.authentication.UserInformationEntity;

@Data
@Builder
@Entity
@Table(name = "participants")
@NoArgsConstructor
@AllArgsConstructor
public class ParticipantEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id", nullable = false)
    private Integer id; // Mã participant

    @ManyToOne
    @JoinColumn(name = "conversation_id", nullable = false, referencedColumnName = "id")
    private ConversationEntity conversation; // Mã cuộc trò chuyện

    @ManyToOne
    @JoinColumn(name = "user_id", nullable = false, referencedColumnName = "id")
    private UserInformationEntity user; // Mã người dùng tham chiếu

    @Column(name = "chat_participants_status", nullable = false)
    private Boolean chatParticipantsStatus; // Đang hoạt động, không hoạt động
}

