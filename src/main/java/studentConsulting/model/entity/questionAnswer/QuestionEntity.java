package studentConsulting.model.entity.questionAnswer;

import java.time.LocalDateTime;
import java.util.Set;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonIgnore;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.entity.departmentField.DepartmentEntity;
import studentConsulting.model.entity.departmentField.FieldEntity;
import studentConsulting.model.entity.notification.NotificationEntity;
import studentConsulting.model.entity.roleBaseAction.RoleAskEntity;

@Data
@Builder
@Entity
@Table(name = "questions")
@NoArgsConstructor
@AllArgsConstructor
public class QuestionEntity {
	@Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(nullable = false, name = "id")
    private Integer id;

    @Column(name = "created_at", nullable = true, updatable = false, columnDefinition = "TIMESTAMP DEFAULT CURRENT_TIMESTAMP")
    private LocalDateTime createdAt;

    @Column(name = "updated_at", nullable = true, columnDefinition = "TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP")
    private LocalDateTime updatedAt;
    @ManyToOne
    @JoinColumn(name = "user_id", nullable = false, referencedColumnName = "id")
    private UserInformationEntity user; // Mã người dùng tham chiếu

    @ManyToOne
    @JoinColumn(name = "department_id", nullable = false, referencedColumnName = "id")
    private DepartmentEntity department; // Mã phòng ban tham chiếu

    @ManyToOne
    @JoinColumn(name = "field_id", nullable = false, referencedColumnName = "id")
    private FieldEntity field; // Mã lĩnh vực tham chiếu

    @ManyToOne
    @JoinColumn(name = "role_ask_id", nullable = false, referencedColumnName = "id")
    private RoleAskEntity roleAsk; // Mã vai trò hỏi tham chiếu

    @ManyToOne
    @JoinColumn(name = "parent_question_id", referencedColumnName = "id")
    private QuestionEntity parentQuestion; // Mã câu hỏi cha tham chiếu

    @Column(name = "title", nullable = false, length = 255)
    private String title; // Tiêu đề câu hỏi

    @Column(name = "content", nullable = false, length = 900)
    private String content; // Nội dung câu hỏi

    @Column(name = "views", nullable = true)
    private Integer views; // Lượt xem

    @Column(name = "file_name", length = 255)
    private String fileName; // Tên file đính kèm

    @Column(name = "status_approval", nullable = true)
    private Boolean statusApproval; // Được chấp nhận, không được chấp nhận

    @Column(name = "status_public", nullable = true)
    private Boolean statusPublic; // Công khai, riêng tư

    @Column(name = "status_delete", nullable = true)
    private Boolean statusDelete; // Đã xóa, chưa xóa

    @OneToMany(mappedBy = "parentQuestion", fetch = FetchType.LAZY, cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<QuestionEntity> questions;

    @OneToMany(mappedBy = "question", fetch = FetchType.LAZY, cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<ForwardQuestionEntity> forwardQuestions;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "question", cascade = CascadeType.ALL)
    private Set<AnswerEntity> answers;

    @OneToMany(mappedBy = "question", fetch = FetchType.LAZY, cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<NotificationEntity> notifications;

    @OneToMany(mappedBy = "question", fetch = FetchType.LAZY, cascade = CascadeType.ALL, orphanRemoval = true)
    @JsonIgnore
    private Set<DeletionLogEntity> deletionLog;
    
    
    
}
