package studentConsulting.model.entity.main;

import javax.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import studentConsulting.model.entity.authentication.UserEntity;

import java.time.LocalDateTime;

@Data
@Builder
@Entity
@Table(name = "answers")
@NoArgsConstructor
@AllArgsConstructor
public class AnswerEntity {

    @Id
    @Column(name = "id", length = 50, nullable = false)
    private String id;

    @Column(name = "content", columnDefinition = "TEXT")
    private String content;

    @Column(name = "date", nullable = false)
    private LocalDateTime date;

    @Column(name = "approved", nullable = false)
    private boolean approved;

    @Column(name = "status", nullable = false)
    private boolean status = true;

    @Column(name = "user_id", length = 50, nullable = false)
    private Long userId;

    @Column(name = "question_id", length = 50, nullable = false)
    private String questionId;

//    @ManyToOne(fetch = FetchType.LAZY)
//    @JoinColumn(name = "user_id", insertable = false, updatable = false)
//    private userEntity user;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "question_id", insertable = false, updatable = false)
    private QuestionEntity question;
}
