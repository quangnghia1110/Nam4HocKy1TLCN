package studentConsulting.entity.main;

import javax.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import studentConsulting.entity.authentication.userEntity;

import java.util.List;

@Data
@Builder
@Entity
@Table(name = "conversations")
@NoArgsConstructor
@AllArgsConstructor
public class conversationEntity {

    @Id
    @Column(name = "id", length = 50, nullable = false)
    private String id;

    @Column(name = "staff_id", length = 50, nullable = false)
    private String staffId;

    @Column(name = "user_id", length = 50, nullable = false)
    private Long userId;

    @Column(name = "deleted_by_staff", nullable = false)
    private boolean deletedByStaff = false;

    @Column(name = "deleted_by_user", nullable = false)
    private boolean deletedByUser = false;

    @Column(name = "status", nullable = false)
    private boolean status = true;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "staff_id", insertable = false, updatable = false)
    private userEntity staff;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "user_id", insertable = false, updatable = false)
    private userEntity user;

    @OneToMany(mappedBy = "conversation", fetch = FetchType.LAZY)
    private List<messageEntity> messages;
}
