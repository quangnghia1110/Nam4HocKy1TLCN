package studentConsulting.model.entity.news;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.*;
import java.time.LocalDateTime;

@Entity
@Table(name = "posts")
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class PostEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    private String content;
    private boolean isAnonymous;
    private String author;
    private String fileName; 
    private boolean isApproved;
    private int views;
    private LocalDateTime createdAt;
    
    public PostEntity(Long id) {
        this.id = id;
    }
}
