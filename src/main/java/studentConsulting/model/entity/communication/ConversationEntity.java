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
@Table(name = "conversations")
@NoArgsConstructor
@AllArgsConstructor
public class ConversationEntity{
	@Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(nullable = false, name = "id")
    private Integer id;

    @Column(name = "created_at", nullable = false, updatable = false, columnDefinition = "TIMESTAMP DEFAULT CURRENT_TIMESTAMP")
    private Timestamp createdAt;

    @Column(name = "updated_at", nullable = false, columnDefinition = "TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP")
    private Timestamp updatedAt;
    @ManyToOne
    @JoinColumn(name = "user_id", nullable = false, referencedColumnName = "id")
    private UserInformationEntity user; // Mã người dùng tham chiếu

    @ManyToOne
    @JoinColumn(name = "consultant_id", nullable = false, referencedColumnName = "id")
    private UserInformationEntity consultant; // Mã tư vấn viên tham chiếu

    @Column(name = "department_id", nullable = false)
    private Integer departmentId; // Mã phòng ban tham chiếu

    @Column(name = "name", nullable = false, length = 255)
    private String name; // Tên room

    @Column(name = "status_active", nullable = false)
    private Boolean statusActive; // Đang hoạt động, kết thúc

    @Column(name = "is_group", nullable = false)
    private Boolean isGroup; // Xác định cuộc trò chuyện có phải là nhóm hay không

    @OneToMany(mappedBy = "conversation", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<MessageEntity> messages; // Danh sách tin nhắn thuộc về cuộc trò chuyện

    @OneToMany(mappedBy = "conversation", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<ParticipantEntity> participants; // Danh sách người tham gia
}


