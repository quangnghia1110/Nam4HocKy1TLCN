package studentConsulting.model.entity.consultation;
import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import studentConsulting.model.entity.address.AddressEntity;
import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.entity.communication.ConversationEntity;
import studentConsulting.model.entity.communication.ParticipantEntity;
import studentConsulting.model.entity.departmentField.DepartmentEntity;

import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.util.Set;

@Data
@Builder
@Entity
@Table(name = "consultation_schedule")
@NoArgsConstructor
@AllArgsConstructor
public class ConsultationScheduleEntity {
	@Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(nullable = true, name = "id")
    private Integer id;

    @Column(name = "created_at", nullable = true, updatable = false, columnDefinition = "TIMESTAMP DEFAULT CURRENT_TIMESTAMP")
    private Timestamp createdAt;

    @Column(name = "updated_at", nullable = true, columnDefinition = "TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP")
    private Timestamp updatedAt;
    @ManyToOne
    @JoinColumn(name = "user_id", nullable = true, referencedColumnName = "id")
    private UserInformationEntity user; // Mã người dùng tham chiếu

    @ManyToOne
    @JoinColumn(name = "consultant_id", nullable = true, referencedColumnName = "id")
    private UserInformationEntity consultant; // Mã người tư vấn tham chiếu

    @Column(name = "title", nullable = true, length = 255)
    private String title; // Tiêu đề

    @Column(name = "content", nullable = true, length = 255)
    private String content; // Nội dung

    @Column(name = "consultation_date", nullable = true)
    private LocalDateTime consultationDate; // Ngày diễn ra

    @Column(name = "consultation_time", nullable = true)
    private LocalDateTime consultationTime; // Khoảng thời gian

    @Column(name = "location", nullable = true, length = 255)
    private String location; // Địa điểm

    @Column(name = "link", nullable = true, length = 255)
    private String link; // Đường dẫn meeting

    @Column(name = "mode", nullable = true)
    private Boolean mode; // Trạng thái lịch online, offline

    @Column(name = "status_confirmed", nullable = true)
    private Boolean statusConfirmed; // Được chấp nhận, không chấp nhận

    @Column(name = "status_public", nullable = true)
    private Boolean statusPublic; // Công khai, riêng tư
    
    @ManyToOne
    @JoinColumn(name = "department_id", nullable = true, referencedColumnName = "id")
    private DepartmentEntity department;
}
