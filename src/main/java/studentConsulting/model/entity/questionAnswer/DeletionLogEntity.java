package studentConsulting.model.entity.questionAnswer;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.*;

import com.fasterxml.jackson.annotation.JsonIgnore;

import java.time.LocalDateTime;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "deletion_log")
public class DeletionLogEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Integer id;

    @ManyToOne
    @JoinColumn(name = "question_id", nullable = false)
    @JsonIgnore
    private QuestionEntity question;

    @Column(nullable = false)
    private String reason;

    @Column(nullable = false)
    private String deletedBy;

    @Column(nullable = false)
    private LocalDateTime deletedAt;
}
